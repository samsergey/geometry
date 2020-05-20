{-# Language TypeApplications #-}
module Main where

import Geometry

pt = aLabel "A" % at' (4,5)
t = aTriangle % scale 8 % scaleX 1.5
c = circle' 4 (6,-6) % label "c"
l = aLine % at' (-3, 5) % along' 100 % dotted % white
ct' = (t <+> c) % reflectAt l


main :: IO ()
main = chart "test.svg" $ t <+> c <+> l <+> ct'
