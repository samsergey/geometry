{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-} 
module Geometry where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Curve
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

point (x, y) = Point (x :+ y)

pointOn c t = Point (c `param` t)

------------------------------------------------------------

circle r (x,y) = mkCircle c (c + (r :+ 0))
  where c = x :+ y

------------------------------------------------------------

line (x1, y1) (x2, y2) = Line (x1:+y1) (x2:+y2)
