{-# language OverloadedStrings #-}

import Geometry
import Data.Complex

path = "dist-newstyle/build/x86_64-linux/ghc-8.6.5/geometry-0.1.2.0/doc/html/geometry/figs/"

main = do
  writeSVG 400 (path <> "line1.svg") $
    aLine # rotate 30 #: "a"

  writeSVG 400 (path <> "line2.svg") $
    line (1,0) (5,3)

  writeSVG 400 (path <> "line3.svg") $
    line' (1 :: CN) (point (5,3))
