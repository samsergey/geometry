{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

c = aCircle # rotate 90

sc = group (modularScale 24 c)

ch = c <+> sc <+> (c # scale 1.1 #: invisible)

main :: IO ()
main = writeSVG 350 "test.svg" ch
