-- Here we do the actual scans
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Data.Array.IO                       as R

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
  
-- We create a list of that which shall be evaluated
data Frame = Frame { fi        :: Int            -- frame index
                   , ti        :: Double         -- time index, based on the number of frames per second
                   , si        :: Double         -- scan index, always increasing

                   , simg1     :: ImageVRD       -- source image 1
                   , foffsimg1 :: Int            -- frame offset for source image 1 
                   
                   , simg2     :: ImageVRD       -- source image 2
                   , foffsimg2 :: Int            -- frame offset for source image 2
                   
                   , intimg1   :: Maybe ImageVRD -- intermediate image 1
                   , intimg2   :: Maybe ImageVRD -- intermediate image 2

                   , imgfile   :: String         -- pathname to the image frame that will be written
                   , slitM     :: MatrixD        -- (computed) slit matrix
                   } deriving Show

-- The following are related to Frame. TODO -- tie these directly to Frame somehow
-- this will become especially useful when we specify multiple images for simg1 as well as simg2. TODO
fi1 f = fi f - foffsimg1 f
fi2 f = fi f - foffsimg2 f

type SSFrameArray = IOArray Int Frame

listOfFrames :: Parms -> ImageVRD -> ImageVRD -> IO SSFrameArray
listOfFrames p i1 i2 = newListArray (0, frames p)
                       [Frame { fi = i
                              , ti = fromIntegral i / frames_per_sec p
                              , si = fromIntegral i * scans_per_sec p / frames_per_sec p
                              , simg1 = i1
                              , simg2 = i2
                              , foffsimg1 = 0 -- TODO - logic to handle multiple images
                              , foffsimg2 = 0 -- TODO - logic to handle multiple images
                              , intimg1 = Nothing
                              , intimg2 = Nothing
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

scanOneFrame :: Parms -> Frame -> IO (ImageVRD, Frame)
scanOneFrame p f = do
  let f' = f { intimg1 = Just $ intermediateSlitShift (intimg1 f) (simg1 f)
             , intimg2 = Just $ intermediateSlitShift (intimg2 f) (simg1 f)
             }
  let icanvas = makeImage (canvas_height p, canvas_width p)
        (\(x, y) -> fromMaybePixel $ pixelScanner x y f')
  return (icanvas, f')
  where
    intermediateSlitShift :: Maybe ImageVRD -> ImageVRD -> ImageVRD
    intermediateSlitShift mim sim | isJust mim = translate Edge (0, 1) $ fromJust $ intimg1 f
                                  | otherwise = makeImage (canvas_height p, canvas_width p) (\(_, _) -> PixelRGB 0 0 0)

    pixelScanner x y f'
      | side == LeftSide   = I.maybeIndex (fromJust $ intimg1 f') $ transformP p f (fromJust $ intimg1 f') Before (x, y)
      | side == RightSide  = I.maybeIndex (fromJust $ intimg2 f') $ transformP p f (fromJust $ intimg2 f') After  (x, y)
      | side == TopSide    = I.maybeIndex (fromJust $ intimg1 f') $ transformP p f (fromJust $ intimg1 f') Before (x, y)
      | side == BottomSide = I.maybeIndex (fromJust $ intimg2 f') $ transformP p f (fromJust $ intimg2 f') After  (x, y)
      where
        side = canvasSide p (x, y) (canvas_height p, canvas_width p)

writeOneFrame :: Parms -> Frame -> IO Frame
writeOneFrame p f = do
  (canvas, f') <- scanOneFrame p f
  writeImage (imgfile f) canvas
  return f'
  
-- What we want to do here is to create a sequence of tuples,
-- which would contain the sequence (frame) number, generated pathname, and the t(ime)
-- parameter.

scanFromParms :: Parms -> IO ()
scanFromParms p = do
  i1' <- I.readImageRGB VU $ img1 p
  i2' <- I.readImageRGB VU $ img2 p

  -- resize everything to canvas dimensions to simplify the math.
  let i1 = resize Bilinear Edge (canvas_height p, canvas_width p) i1'
  let i2 = resize Bilinear Edge (canvas_height p, canvas_width p) i2'
  
  frms <- listOfFrames p i1 i2
  (a, b) <- getBounds frms
  _ <- computeFrames p frms a b
  return ()
  where
    computeFrames :: Parms -> SSFrameArray -> Int -> Int -> IO ()
    computeFrames _ _ _ 0 = return ()
    computeFrames p frms i b = do
      frm  <- readArray frms i
      frm' <- writeOneFrame p frm
      writeArray frms i frm'
      verboseDump p frm
      computeFrames p frms (i+1) (b-1)
        where
          verboseDump p f = do
             if verbose p
               then print $ imgfile f
               else return ()

