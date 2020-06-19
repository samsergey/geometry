{-# language DerivingVia #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Geometry.Point
  (-- ** Types
    APoint(..), Point (..), Label (..)
    -- ** Constructors
  , mkPoint, mkLabel
  ) where

import Data.Complex
import Data.Bool

import Geometry.Base

------------------------------------------------------------

class APoint p where
  toPoint :: p -> Point
  asPoint :: Point -> p

instance APoint CN where
  toPoint = Point 
  asPoint (Point p) = p

instance APoint XY where
  toPoint = Point . cmp
  asPoint (Point p) = coord p

instance APoint p => APoint (Maybe p) where
  toPoint = maybe (asCmp 0) toPoint
  asPoint = Just . asPoint

------------------------------------------------------------

-- | The type representing a point.
newtype Point = Point CN

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
  p1 == p2 = cmp p1 ~== cmp p2

instance Trans Point where
  transform t (Point p) = Point $ transformCN t p 

instance Affine Point where
  cmp (Point p) = p
  asCmp = Point

instance Figure Point where
  isTrivial _ = False
  refPoint = cmp
  box = pointBox
          
------------------------------------------------------------

-- | The type representing a label.
newtype Label = Label CN
  deriving (Eq, Trans, Affine, Figure, APoint) via Point

-- | The general label constructor (the label test is set by `label` decorator).
mkLabel :: Affine a => a -> Label
mkLabel = Label . cmp

instance Show Label where
  show p = concat ["<Label (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

