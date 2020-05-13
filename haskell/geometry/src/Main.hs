{-# Language TypeApplications #-}
module Main where

import Base
import Affine
import Point
import Circle
import Line
import Geometry
import SVG


ch :: Group
ch = let p = point @XY (2, 3)
         c = circle @XY 3 (1, 2)
         l = line p (center c)
     in p <+> c <+> l


main :: IO ()
main = do
  putStrLn "hello world"
