{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

c = aTriangle

sc = group (modularScale 3 c)

ch = c <+> sc <+>  (c # scale 1.1 #: invisible)

main :: IO ()
main = writeSVG 350 "test.svg" ch
