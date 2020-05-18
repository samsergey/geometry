{-# Language TypeApplications #-}
module Main where

import Geometry

p = point @XY (2, 3)
c = circle @XY 3 (1, 2)
t = regularPoly 3 <| scale 8
pt = group [point (t .@ x) | x <- [0,0.1..1]]
l = line p (center c)
l2 = group [ aLine <| at' (20, y) <| along (asDeg 15)
           | y <- [-20..20]]

main :: IO ()
main = chart "test.svg" $ plane <+> c <+> l <+> t <+> pt
