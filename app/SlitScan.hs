{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

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

import CommandLine
import Scan
         
main :: IO ()
main = do
  parms <- commandline
  print parms
  --i1 <- I.readImageRGB VU $ img1 parms
  --i2 <- I.readImageRGB VU $ img2 parms
  --print i1
  --print i2
  --let p = I.index i1 (10, 10)
  --print p
  scanFromParms parms  
