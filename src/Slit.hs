-- Slit of an image
module Slit 
    ( Slit (..)
    ) where

-- Not all of the following are needed here! FIX
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

