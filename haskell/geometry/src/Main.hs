{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

pt = point (14,5) #: "B"

lb = aLabel "A" # at (-10,0)

t = aTriangle # scale 8 . scaleX 1.5

c = circle 4 (6,-6) #: "c"

l = aLine
    #: "a" <> dotted <> white
    # at (-3, 8) # along 45

ct' = (t <+> c) # reflectAt l 

s = aRay # at' pt # normalTo l

an = angleBetween s l
        
t1 = regularPoly 25 # scale 1 # on l 0

main :: IO ()
main = writeSVG "test.svg" (t1 <+> l)

