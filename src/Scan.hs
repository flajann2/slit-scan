-- Here we do the actual scans

module Scan
  ( scanFromParms
  , ImageVRD
  , PixelVRD
  ) where

import Slit
import CommandLine(Parms(..))

import Control.Monad
import Criterion.Main
import Data.Array.Repa                     as R
import Data.Array.Repa.Algorithms.Convolve as R
import Data.Array.Repa.Eval                as R
import Data.Array.Repa.Repr.Unboxed        as R
import Data.Array.Repa.Stencil             as R
import Data.Array.Repa.Stencil.Dim2        as R

import Graphics.Image                      as I
import Graphics.Image.IO                   as I
import Graphics.Image.Interface            as I
import Graphics.Image.Interface.Repa
import Prelude                             as P

data CanvasSide = LeftSide
                | RightSide
                | TopSide
                | BottomSide deriving (Eq, Show)

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

scanOneFrame :: Parms -> Double -> ImageVRD -> ImageVRD -> IO ImageVRD
scanOneFrame p t i1 i2 = do
  let icanvas = makeImage (canvas_height p, canvas_width p)
        (\(x, y) -> fromMaybePixel $ pixelScanner x y)
  return icanvas
  where
    pixelScanner x y
      | side == LeftSide   = I.maybeIndex i1 (x, y)
      | side == RightSide  = I.maybeIndex i2 (x, y)
      | side == TopSide    = I.maybeIndex i1 (x, y)
      | side == BottomSide = I.maybeIndex i2 (x, y)
      where
        side = canvasSide p (x, y) (canvas_height p, canvas_width p)


scanFromParms :: Parms -> IO ()
scanFromParms p = do
  i1 <- I.readImageRGB VU $ img1 p
  i2 <- I.readImageRGB VU $ img2 p
  can <- scanOneFrame p 0 i1 i2
  writeImage "foo.png" can 
  return ()
