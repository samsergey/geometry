{-# Language TypeApplications #-}
module Main where

import Geometry

p = point' (2, 3)
c = circle' 3 (1, 2)
t = regularPoly 3 <| scale 8
l = line p (center c) <| labeled "a" 
l2 = group [ aLine <| at' (20, y) <| along (asDeg 15)
           | y <- [-20..20]]

main :: IO ()
main = chart "test.svg" $ c <+> l <+> t <+> p
