-- Here we do the actual scans
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Scan
  ( scanFromParms
  , ImageVRD
  , PixelVRD
  , ParametricSlit
  ) where

import Slit
import CommandLine (Parms(..))

import Control.Monad
import Control.Concurrent.Async
import Criterion.Main
import qualified Data.Array.Repa           as R
import Data.Array.Repa.Algorithms.Convolve as R
import Data.Array.Repa.Eval                as R
import Data.Array.Repa.Repr.Unboxed        as R
import Data.Array.Repa.Stencil             as R
import Data.Array.Repa.Stencil.Dim2        as R
import Data.Functor
--import Data.Array.IO                       as R
import Data.IORef                          as R
import Data.Foldable                       as R

import Numeric.LinearAlgebra               as N
import Algebra.RealRing (fraction)

import Graphics.Image                      as I
import Graphics.Image.IO                   as I
import Graphics.Image.Interface            as I
import Graphics.Image.Interface.Repa       as I

import System.FilePath.Posix               as F

import Prelude                             as P
import Data.Maybe                          as P

import Formatting                          as F
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as TLB

import Debug.Trace                         as D -- TODO this goes away in production

data CanvasSide = LeftSide
                | RightSide
                | TopSide
                | BottomSide deriving (Eq, Show)

-- before or after the slit? Before would be above if the slit is horizontal,
-- to the left if the slit is verical. 
data SlitSide = Before | After deriving (Eq, Show)

canvasSide :: Parms -> (Int, Int) -> (Int, Int) -> CanvasSide
canvasSide p (x, y) (rows, cols)
  | vertDir == True = verticalSlit
  | otherwise       = horizontalSlit
  where
    vertDir = vert p
    verticalSlit   
      | x*2 < rows = LeftSide
      | otherwise  = RightSide
    horizontalSlit 
      | y*2 < cols = TopSide
      |otherwise   = BottomSide

fromMaybePixel :: Maybe PixelVRD -> PixelVRD
fromMaybePixel Nothing  = PixelRGB 0 0 0
fromMaybePixel (Just x) = x

-- ParametricSlit for slit generation (simple version)
instance ParametricSlit NPoint Double Int where
  paraSlit (NPoint(x1,y1)) (NPoint(x2,y2)) q = NPoint( x1 + (x2-x1) * q
                                                     , y1 + (y2-y1) * q)
  swapComp (NPoint(x,y)) = NPoint(y,x) -- we do this when the slit is flipped from horizontal to vertica
  frameIndexToSlit ix = undefined
  
-- We do the ioref on the intermediate image since we must constantly update it 
type IntImageVRD = IORef ImageVRD

-- We create a list of that which shall be evaluated
data Frame = Frame { fi        :: Int            -- frame index
                   , ti        :: Double         -- time index, based on the number of frames per second
                   , si        :: Double         -- scan index, always increasing

                   , simg1     :: ImageVRD       -- source image 1
                   , foffsimg1 :: Int            -- frame offset for source image 1 
                   
                   , simg2     :: ImageVRD       -- source image 2
                   , foffsimg2 :: Int            -- frame offset for source image 2
                   
                   , intimg1   :: IntImageVRD    -- intermediate image 1
                   , intimg2   :: IntImageVRD    -- intermediate image 2

                   , imgfile   :: String         -- pathname to the image frame that will be written
                   , slitM     :: MatrixD        -- (computed) slit matrix
                   } deriving Show

instance Show IntImageVRD where
  show a = show $ readIORef a
  
-- The following are related to Frame. TODO -- tie these directly to Frame somehow
-- this will become especially useful when we specify multiple images for simg1 as well as simg2. TODO
fi1 f = fi f - foffsimg1 f
fi2 f = fi f - foffsimg2 f

--type SSFrameArray = IOArray Int Frame

listOfFrames :: Parms -> ImageVRD -> ImageVRD -> IntImageVRD -> IntImageVRD -> [Frame]
listOfFrames p src1 src2 intm1 intm2 = [Frame { fi = i
                                              , ti = fromIntegral i / frames_per_sec p
                                              , si = fromIntegral i * scans_per_sec p / frames_per_sec p
                                              , simg1 = src1
                                              , simg2 = src2
                                              , foffsimg1 = 0 -- TODO - logic to handle multiple images
                                              , foffsimg2 = 0 -- TODO - logic to handle multiple images
                                              , intimg1 = intm1
                                              , intimg2 = intm2
                                              , imgfile = out p ++ formatToString ("_" % left 4 '0' % "." % string) i (image_format p)
                                              , slitM = slitMatrix p
                                              } | i <- [0 .. frames p]]

transformP :: Parms -> Frame -> ImageVRD -> SlitSide -> (Int, Int) -> (Int, Int)
transformP p f im ss (x, y) = transformToTup
  where
    npoint = toNrc (PPoint(x,y)) (I.rows im, I.cols im)
    scanV = 3 |>[si f, 0, 0]
    npoint' = vToN $ scanV + slitM f #> nToV npoint
    transformToTup = toTup $ toP npoint' im

makeImageVRD :: Parms -> ImageVRD
makeImageVRD p = makeImage (canvas_height p, canvas_width p) (\(_, _) -> PixelRGB 0 0 0)

scanOneFrame :: Parms -> Frame -> IO ImageVRD
scanOneFrame p f = do
  im1 <- readIORef $ intimg1 f
  im2 <- readIORef $ intimg2 f
  
  writeIORef (intimg1 f) $ intermediateSlitShift im1 $ simg1 f
  writeIORef (intimg2 f) $ intermediateSlitShift im2 $ simg2 f

  im1' <- readIORef $ intimg1 f
  im2' <- readIORef $ intimg2 f
 
  let icanvas = makeImage (canvas_height p, canvas_width p)
        (\(x, y) -> fromMaybePixel $ pixelScanner x y f im1' im2')
  print f
  return icanvas
  where
    intermediateSlitShift :: ImageVRD -> ImageVRD -> ImageVRD
    intermediateSlitShift mim sim = catAndChop min
      where
       catAndChop im = backpermute (canvas_height p, slit_width p) (slitMapper sim) sim
         where
           slitMapper :: ImageVRD -> (Int, Int) -> (Int, Int)
           slitMapper im (x, y) = toTup $ toP  (slitMapperN $ toN (PPoint(x, y)) im) im

           slitMapperN :: NPoint -> NPoint
           slitMapperN (NPoint(x, y)) = NPoint(x, y)
 
    pixelScanner x y f' im1' im2'
      | side == LeftSide   = I.maybeIndex im1' $ transformP p f im1' Before (x, y)
      | side == RightSide  = I.maybeIndex im2' $ transformP p f im2' After  (x, y)
      | side == TopSide    = I.maybeIndex im1' $ transformP p f im1' Before (x, y)
      | side == BottomSide = I.maybeIndex im2' $ transformP p f im2' After  (x, y)
      where
        side = canvasSide p (x, y) (canvas_height p, canvas_width p) 

writeOneFrame :: Parms -> Frame -> () -> IO ()
writeOneFrame p f _ = do
  canvas <- scanOneFrame p f
  writeImage (imgfile f) canvas
  
-- What we want to do here is to create a sequence of tuples,
-- which would contain the sequence (frame) number, generated pathname, and the t(ime)
-- parameter.

scanFromParms :: Parms -> IO ()
scanFromParms p = do
  si1' <- I.readImageRGB VU $ img1 p
  si2' <- I.readImageRGB VU $ img2 p

  -- resize everything to canvas dimensions to simplify the math.
  let src1 = resize Bilinear Edge (canvas_height p, canvas_width p) si1'
  let src2 = resize Bilinear Edge (canvas_height p, canvas_width p) si2'

  intm1 <- newIntImageVRD p
  intm2 <- newIntImageVRD p
  
  let frms = listOfFrames p src1 src2 intm1 intm2
  computeFrames p frms
  where
    computeFrames :: Parms -> [Frame] -> IO ()
    computeFrames p frms = do
      foldrM (writeOneFrame p) () frms
    newIntImageVRD p = newIORef $ makeImageVRD p
