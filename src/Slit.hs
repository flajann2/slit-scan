-- Slit of an image
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Slit 
    ( toP
    , toN
    , NPoint
    , PPoint
    , ImageVRD
    ) where


import CommandLine(Parms(..))
  
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

import Numeric.LinearAlgebra               as N
import Numeric.LinearAlgebra.Data          as N

type ImageVRD = I.Image VU RGB Double
newtype NPoint = NPoint (Double, Double) deriving (Show, Eq)
newtype PPoint = PPoint (Int, Int)       deriving (Show, Eq)

-- convert a Physical point to a Normalised point
toN :: PPoint -> ImageVRD -> NPoint
toN (PPoint (x,y)) im = NPoint( fromIntegral x / fromIntegral (I.rows im)
                              , fromIntegral y / fromIntegral (I.cols im))
 
-- convert a normalised point to a physical point
toP :: NPoint -> ImageVRD -> PPoint 
toP = undefined

-- transformation equations

class SquareArrows p a where
  ax, ay, bx, by, cx, cy, dx, dy :: p -> a

instance SquareArrows Parms Double where
  ax p = 0
  ay p = 0
  bx p = 0
  by p = 1
  cx p = 1
  cy p = - expand p
  dx p = 1
  dy p = expand p
   
class TransformMatrix p a where
  m11, m12, m13, m21, m22, m23, m31, m32, m33 :: p -> a

instance TransformMatrix Parms Double  where
  m11 p = (1 + m31 p) * cx p - ax p
  m12 p = (1 + m32 p) * bx p - ax p
  m13 p = undefined
  
  m21 p = (1 + m31 p) * cy p - ay p
  m22 p = (1 + m32 p) * by p - ay p
  m23 p = undefined 
  
  m31 p = undefined
  m32 p = undefined
  m33 p = undefined

