{-# Language TypeApplications #-}
module Main where

import Geometry

p = group [ aRay
            <| scale 20
            <| along (asDeg x)
            <| label "a"
          | x <- [0,30..150] ]


l = aLine
    <| scale 20
    <| along (asDeg 30)
    <| label "A"
    <| lparam 0.9
    
--lb = aLabel "Text" <| at' (-4,4)
--c = circle' 3 (1, 2) <| label "c"
--t = aTriangle <| scale 8
--l = line p (center c) <| label "a"
--l2 = group [ aLine <| at' (20, y) <| along (asDeg 15)
--           | y <- [-20..20]]

main :: IO ()
main = chart "test.svg" $ p
