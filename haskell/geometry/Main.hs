{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

c = aCircle # rotate 90

sc = group (modularScaleOn c 24)

ch = c <+> sc <+> (c # scale 1.1 #: invisible)

main :: IO ()
main = writeSVG 350 "test.svg" ch
