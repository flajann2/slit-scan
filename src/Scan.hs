-- Here we do the actual scans

module Scan
  ( scanFromParms
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

scanFromParms :: Parms -> IO ()
scanFromParms p = do
  i1 <- I.readImageRGB VU $ img1 p
  i2 <- I.readImageRGB VU $ img2 p
  return ()
