{-|
Module      : W
Description : Reexporting all internal stuff.
Stability   : experimental
-}
module Geometry
( 
  writeSVG, showSVG, (<+>), group, beside, above, (<||>), Group (..)
  , put, chart
  , module Geometry.Base
  , module Geometry.Figures
  , module Geometry.Point
  , module Geometry.Line
  , module Geometry.Circle
  , module Geometry.Polygon
  , module Geometry.Angle
  , module Geometry.Decorations
  , module Geometry.Intersections
)
where

import Prelude hiding (writeFile)
import Data.Text.Lazy.IO (writeFile)

import Geometry.Base
import Geometry.Point
import Geometry.Circle
import Geometry.Line
import Geometry.Polygon
import Geometry.Angle
import Geometry.Decorations
import Geometry.Intersections
import Geometry.Figures
import Geometry.SVG

------------------------------------------------------------

-- | Creates SVG for a SVGable object and writes to a file with a given name.
writeSVG :: (Figure a, SVGable a) => ImageSize -> FilePath -> a -> IO ()
writeSVG size name = writeFile name . showSVG size


