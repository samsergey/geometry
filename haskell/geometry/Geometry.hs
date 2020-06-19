{-|
Module      : W
Description : Reexporting all internal stuff.
Stability   : experimental
-}
module Geometry
( -- * Main interface
  writeSVG, showSVG, (<+>), group, beside, abowe, (<||>)
  -- ** Definitions for figures and modifiers
  , module Geometry.Figures
  -- ** Main classes  for affine computations and transformations
  , module Geometry.Base
   -- ** Definitions of a point-like primitives.
  , module Geometry.Point
   -- ** Definitions of a line-like primitives.
  , module Geometry.Line
   -- ** Definition of a circle primitive.
  , module Geometry.Circle
   -- ** Definitions of a polygons and triangles.
  , module Geometry.Polygon
   -- ** Definitions of an angle mark
  , module Geometry.Angle
   -- ** Style settings.
  , module Geometry.Decorations
   -- ** Curve intersections.
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


