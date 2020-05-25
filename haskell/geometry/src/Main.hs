{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

pt = point (14,5) #: "B"

lb = aLabel "A" # at (-10,0)

t = aTriangle # scale 8 . scaleX 1.5
    
c = circle 4 (6,-6) #: "c"

l = aLine
    #: "a" <> dotted <> white
    # at (-3, 8) # along 45

ct' = (t <+> c) # reflectAt l

s = aSegment # scale 5 # at' pt # perpendicularTo l

main :: IO ()
main = writeSVG "test.svg" $ lb <+> t <+> c <+> l <+> ct' <+> pt  <+> s

