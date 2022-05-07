-- Slit of an image
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Slit 
    ( toP
    , toN
    , NPoint
    , PPoint
    , ImageVRD
    ) where

-- Not all of the following are needed here! FIX
import Control.Applicative

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

import Generics.OneLiner

type ImageVRD = I.Image VU RGB Double
newtype NPoint = NPoint (Double, Double) deriving (Show, Eq)
newtype PPoint = PPoint (Int, Int)       deriving (Show, Eq)

-- convert a Physical point to a Normalised point
toN :: PPoint -> ImageVRD -> NPoint
toN (PPoint (x,y)) im = NPoint(fromIntegral x / fromIntegral (rows im)
                                     , fromIntegral y / fromIntegral (cols im))
 
-- convert a normalised point to a physical point
toP :: NPoint -> ImageVRD -> PPoint 
toP = undefined
