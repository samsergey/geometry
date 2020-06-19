module Geometry
( -- * Main interface
  writeSVG, showSVG, (<+>), group, beside, abowe, (<||>)
  -- ** Definitions for figures and modifiers
  , module Figures
  -- ** Main classes  for affine computations and transformations
  , module Base
   -- ** Definitions of a point-like primitives.
  , module Point
   -- ** Definitions of a line-like primitives.
  , module Line
   -- ** Definition of a circle primitive.
  , module Circle
   -- ** Definitions of a polygons and triangles.
  , module Polygon
   -- ** Definitions of an angle mark
  , module Angle
   -- ** Style settings.
  , module Decorations
   -- ** Curve intersections.
  , module Intersections
)
where

import Prelude hiding (writeFile)
import Data.Text.Lazy.IO (writeFile)

import Base
import Point
import Circle
import Line
import Polygon
import Angle
import Decorations
import Intersections
import Figures
import SVG

------------------------------------------------------------

-- | Creates SVG for a SVGable object and writes to a file with a given name.
writeSVG :: (Figure a, SVGable a) => ImageSize -> FilePath -> a -> IO ()
writeSVG size name = writeFile name . showSVG size


