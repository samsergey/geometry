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
    #: "a"
    # at (-3, 8) # along 45

ct' = (t <+> c) # reflectAt l 

s = aSegment # scale 8 # at' pt # normalTo l #: "s"

an = angleBetween s l #: arcs 2
        
t1 = regularPoly 5 # scale 5 # on l 0

main :: IO ()
main = writeSVG "test.svg" (t1 <+> l <+> c <+> t <+> pt <+> lb <+> an)
