-- Here we handle most of the math behind doing slits of images
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Slit 
    ( toP
    , toNrc
    , toN
    , toTup
    , NPoint(..)
    , PPoint(..)
    , ImageVRD
    , PixelVRD
    , nToV
    , vToN
    , MatrixD
    , slitMatrix
    ) where

import CommandLine( Parms(..)
                  , sampleParms
                  )
  
-- Not all of the following are needed here! FIX
import Control.Applicative

import Control.Monad
import Criterion.Main

import Data.Array.Repa hiding ((!))    --  as R
import Data.Array.Repa.Algorithms.Convolve as R
import Data.Array.Repa.Eval                as R
import Data.Array.Repa.Repr.Unboxed        as R
import Data.Array.Repa.Stencil             as R
import Data.Array.Repa.Stencil.Dim2        as R

import Graphics.Image                      as I
import Graphics.Image.IO                   as I
import Graphics.Image.Interface            as I
import Graphics.Image.Interface.Repa       as I

import Prelude                             as P

import Numeric.LinearAlgebra               as N
import Numeric.LinearAlgebra.Data          as N

type ImageVRD = I.Image VU RGB Double
type PixelVRD = I.Pixel RGB Double

newtype NPoint = NPoint (Double, Double) deriving (Show, Eq)
newtype PPoint = PPoint (Int, Int)       deriving (Show, Eq)

-- convert a Physical point to a Normalised point
toNrc :: PPoint -> (Int, Int) -> NPoint
toNrc (PPoint(x,y)) (rows, cols) = NPoint( fromIntegral x / fromIntegral rows
                                         , fromIntegral y / fromIntegral cols)

toN :: PPoint -> ImageVRD -> NPoint
toN p im = toNrc p (I.rows im, I.cols im)

--  NPoint( fromIntegral x / fromIntegral (I.rows im)
--                              , fromIntegral y / fromIntegral (I.cols im))
 
-- convert a normalised point to a physical point
toP :: NPoint -> ImageVRD -> PPoint 
toP (NPoint (x,y)) im = PPoint( round (x * fromIntegral (I.rows im))
                              , round (y * fromIntegral (I.cols im)))

-- convert a PPoint to a tuple
toTup :: PPoint -> (Int, Int)
toTup (PPoint(x,y)) = (x, y)

-- convert an NPoint to a 3-vector
type VectorD = N.Vector R
nToV :: NPoint -> VectorD
nToV (NPoint (x, y)) = 3 |> [x, y, 1]

-- convert a 3-vector to an NPoint
vToN :: VectorD -> NPoint
vToN vec = NPoint(vec ! 0 / vec ! 2, vec ! 1 / vec ! 2)

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
  dy p = expand p + (by p - ay p) -- / 2
   
class TransformMatrix p a where
  m11, m12, m13, m21, m22, m23, m31, m32, m33 :: p -> a

instance TransformMatrix Parms Double  where
  m11 p = (1 + m31 p) * cx p - ax p
  m12 p = (1 + m32 p) * bx p - ax p
  m13 p = ax p
  
  m21 p = (1 + m31 p) * cy p - ay p
  m22 p = (1 + m32 p) * by p - ay p
  m23 p = ay p 
  
  m31 p =   ((ax p - cx p) * (by p - dy p) - (ay p - cy p) * (bx p - dx p))
          / ((by p - dy p) * (cx p - dx p) - (bx p - dx p) * (cy p - dy p))

  m32 p =   ((ay p - by p) * (cx p - dx p) - (ax p - bx p) * (cy p - dy p))
          / ((by p - dy p) * (cx p - dx p) - (bx p - dx p) * (cy p - dy p))
 
  m33 p = 1

type MatrixD = Matrix Double

slitMatrix :: Parms -> MatrixD
slitMatrix p = (3><3) [ m11 p, m12 p, m13 p
                      , m21 p, m22 p, m23 p
                      , m31 p, m32 p, m33 p
                      ]

