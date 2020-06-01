{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

pt = point (2,2) #: "B"

lb = aLabel #: "A" # at (-10,0)

t = aTriangle # scaleX 1.5

c = aCircle # scale 1 # at (1,0.5)
c2 = aCircle # at (1,1)

l = aLine
    #: "a" <> white <> dotted
    # at (0, 0.5) # along 35

c' = (c <+> t) # reflectAt l 

s = aSegment # at' pt # along 100 #: "s"

an = angleBetween s l #: "#"

ch = s <+> l <+> an

main :: IO ()
main = writeSVG 450 "test.svg" ch
