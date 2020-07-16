{-# language DerivingVia #-}
{-# language FlexibleInstances #-}

module Geometry.Point
  ( APoint(..), Point (..), Label (..)
  , mkPoint, mkLabel 
  ) where

import Data.Complex
import Data.Bool
import Numeric
import Geometry.Base

-------------------------------------------------------------------------------

-- | The transparent class fot point-like objects.
class APoint p where
  toPoint :: p -> Point
  asPoint :: Point -> p

instance APoint Cmp where
  toPoint = Point 
  asPoint (Point p) = p

instance APoint XY where
  toPoint = Point . cmp
  asPoint (Point p) = xy p

instance APoint p => APoint (Maybe p) where
  toPoint = maybe (asCmp 0) toPoint
  asPoint = pure . asPoint

--------------------------------------------------------------------------------

-- | The type representing a point.
newtype Point = Point Cmp

-- | The general point constructor.
mkPoint :: Affine a => a -> Point
mkPoint = Point . cmp

instance APoint Point where
  toPoint = id
  asPoint = id

instance Show Point where
  show p = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Eq Point where
  p1 == p2 = cmp p1 ~= cmp p2

instance Metric Point where
  dist p1 p2 = cmp p1 `dist` cmp p2
  dist2 p1 p2 = cmp p1 `dist2` cmp p2

instance Trans Point where
  transform t (Point p) = Point $ transformCmp t p 

instance Affine Point where
  cmp (Point p) = p
  asCmp = Point

instance Figure Point where
  isTrivial _ = False
  refPoint = cmp
  box = pointBox
          
--------------------------------------------------------------------------------
-- | The type representing a label.
newtype Label = Label Cmp
  deriving ( Eq
           , Metric
           , Trans
           , Affine
           , Figure
           , APoint) via Point

-- | The general label constructor (the label test is set by @label@ decorator).
mkLabel :: Affine a => a -> Label
mkLabel = Label . cmp

instance Show Label where
  show p = concat ["<Label (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

