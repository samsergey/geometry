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
    # at (-3, 5) . along 100 

ct' = (t <+> c) # reflectAt l

s = segment (3,4) (8,13) #: "s"

main :: IO ()
main = chart "test.svg" $ lb <+> t <+> c <+> l <+> ct' <+> pt <+> s

