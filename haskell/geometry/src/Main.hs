{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

pt = point (2,2) #: "B"

lb = aLabel "A" # at (-10,0)

t = aTriangle # scaleX 1.5

c = aCircle

l = aLine
    #: "a" <> white <> dotted
    # at (-1, 1) # along 55

c' = c # reflectAt l 

s = aSegment # scale 8 # at' pt # normalTo l #: "s"

an = angleBetween s l #: arcs 2
        
t1 = regularPoly 5 # on c 0

ch = c <+> t <+> c' <+> l <+> pt

main :: IO ()
main = writeSVG 450 "test.svg" $ ch
