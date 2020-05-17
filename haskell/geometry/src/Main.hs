{-# Language TypeApplications #-}
module Main where

import Geometry

p = point' (2, 3)
c = circle' 3 (1, 2)
t = regularPoly 3 <| scale 8
l = line p (center c)
l2 = aLine <| at ((1, 1) :: XY) <| along (asDeg 30)

main :: IO ()
main = chart "test.svg" $ plane <+> c <+> l2 <+> t <+> p
