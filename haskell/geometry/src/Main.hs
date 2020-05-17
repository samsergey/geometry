{-# Language TypeApplications #-}
module Main where

import Geometry

ch :: Group
ch = let p = point @XY (2, 3)
         c = circle @XY 3 (1, 2)
         l = line p (center c)
         t = regularPoly 3 <| scale 8
     in p <+> c <+> l <+> t

main :: IO ()
main = chart "test.svg" $ ch
