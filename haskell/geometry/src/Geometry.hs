module Geometry
(  -- * Reexports modules
    module Base
  , module Point
  , module Line
  , module Circle
  , module Polygon
  , module Angle
  , module Decorations
  , module Figures
  -- * Main interface
  , writeSVG, showSVG, (<+>), group
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
import Figures
import SVG

------------------------------------------------------------

-- | Creates SVG for a SVGable object and writes to a file with a given name.
writeSVG :: (Figure a, SVGable a) => ImageSize -> FilePath -> a -> IO ()
writeSVG size name = writeFile name . showSVG size


