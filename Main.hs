{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

c = Plot (\x -> (cos (2.5*x), sin (3*x))) (0,4.1*pi)

ch = asPolyline c

main :: IO ()
main = writeSVG 350 "test.svg" ch
