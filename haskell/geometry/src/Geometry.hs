{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, TypeApplications #-} 
module Geometry where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Base
import Point
import Circle
import Line
import Transform
import SVG

------------------------------------------------------------

data Group where 
    Nil :: Group
    Cons :: (Show a, SVGable a, Trans a) => a -> Group -> Group
    Append :: Group -> Group -> Group

infixr 5 <+>
a <+> b = Append (Cons a Nil) (Cons b Nil)


instance Trans Group where
    transform t Nil = Nil
    transform t (Cons x xs) = Cons (transform t x) (transform t xs)
    transform t (Append x xs) = Append (transform t x) (transform t xs)


instance SVGable Group where
    toSVG Nil = mempty
    toSVG (Cons x xs) = toSVG x <> toSVG xs
    toSVG (Append x xs) = toSVG x <> toSVG xs


instance Show Group where
    show Nil = mempty
    show (Cons x xs) = show x <> show xs
    show (Append x xs) = show x <> show xs

------------------------------------------------------------
point :: Pos a => a -> Point
point p = Point (pos p)

origin = Point 0

pointOn :: Curve a => a -> Number -> Point
pointOn c t = Point (c `param` t)

------------------------------------------------------------
circle :: Pos a => Number -> a -> Circle
circle r p = mkCircle c $ c + (r :+ 0)
  where c = pos p

------------------------------------------------------------
line :: (Pos a1, Pos a2) => a1 -> a2 -> Line
line p1 p2 = Line (pos p1) (pos p2)

ch = let p = point @XY (2, 3)
         c = circle @XY 3 (1, 2)
         l = line p (center c)
     in p <+> c <+> l
