{-|
Module      : W
Description : Reexporting all internal stuff.
Stability   : experimental
-}
module Geometry
(
  -- * Main interface
  writeSVG, showSVG,
  -- * Grouping combinators
  (<+>), group, beside, above, (<||>)
  , Groupable (..), Group (..)
--  , put, chart
  -- * Constructors for geometric objects
  -- ** Figures
  , Figure (..), Box
  -- ** Figures' size and bounding box corners.
  , figureHeight, figureWidth
--  , corner, left, right, lower, upper
  -- ** Point constructors
  , APoint(..), Point (..), Label (..)
  , origin
  , aPoint, aLabel
  , point, point'
  , pointOn, projectOn, intersectionPoints, closestTo
  -- ** Line constructors
  , Linear (..), Line (..), Ray (..), Segment (..)
  , aLine, aRay, aSegment, oX, oY
  , line, line', ray, ray', segment, segment'
  , midPerpendicular
  , extendToLength, extendTo, normalSegment, heightFrom, clipBy
  -- ** Angle constructors
  , Angular (..), Angle(..)
  , anAngle
  , angleBetween, angleWithin, bisectrisse
  , supplementary, vertical, reflex
  -- ** Polygon constructors
  , PiecewiseLinear (..)
  , isDegenerate
  , Polyline (..)
  , Polygonal, Polygon (..)
  , closePolyline
  , Triangle (..), aTriangle, triangle2a
  , Rectangle (..), aSquare, aRectangle, space
  , parametricPoly, polarPoly, regularPoly
  , boxRectangle
  -- ** Circle constructors
  , Circular (..), Circle
  , aCircle, circle, circle'
  -- ** Scalers
  , linearScale, modularScale
  -- * Modificators
  -- ** Linear transformations
  , TMatrix, Trans (..)
  , translate', translate, superpose, at, at'
  , rotate, rotateAt', reflect, reflectAt
  , along, along', scale, scaleX, scaleY, scaleAt', scaleAt
  , scaleXAt', scaleXAt, scaleYAt', scaleYAt, scaleFig
  -- ** Other modificators
  , through', through
  , on, normalTo, flipAt
  , vertexAngle, height
  -- * Decorations
  , WithOptions (..)
  , Options, Option(..)
  , Decorated(..)
  -- ** Decorators
  , Decorator(..)
  , (#:)
  , visible, invisible
  , stroke, white, fill
  , thickness, thin
  , dashed, dotted
  , arcs
  , label, loffs, lpos, lparam
  -- * General classes and data types
  -- ** Coordinate represenations
  , Cmp, XY
  -- ** Directed values
  , Direction (..)
  -- *** Direction isomorphisms
  , deg, asDeg
  , rad, asRad
  , turns, asTurns
  -- ** Points in affine space
  , Affine (..), asAffine
  -- *** Predicates
  , isOrthogonal, isCollinear, isOpposite, isZero
  -- *** Vector and point operations
  , dot, det, cross, norm, distance, angle, normalize, columns, azimuth
  -- ** Manifolds and curves
  , Manifold (..), Bounding (..)
  , (->@), (->@?), (@->), (@->?)
  , start, end, paramL, projectL, distanceTo
  , plotManifold
  , Plot (..)
  , Curve (..), PointLocation (..), ClosedCurve(..)
  -- ** Intersections of curves
  , Intersections (..)
  , intersections, isIntersecting
  -- * Miscellaneous classes and functions
  , SVGable (..), SVGContext(..)
  -- ** Fuzzy equality
  , AlmostEq
  , (~<=), (~>=), (~==)
  -- ** Utility fubctions and operators
  , (#)
)
where

import Prelude hiding (writeFile)
import Data.Text.Lazy.IO (writeFile)

import Geometry.Base
import Geometry.Point
import Geometry.Circle
import Geometry.Line
import Geometry.Polygon
import Geometry.Plot
import Geometry.Angle
import Geometry.Decorations
import Geometry.Intersections
import Geometry.Figures
import Geometry.SVG

------------------------------------------------------------

-- | Creates SVG for a SVGable object and writes to a file with a given name.
writeSVG :: (Figure a, SVGable a) => Int -> FilePath -> a -> IO ()
writeSVG size name = writeFile name . showSVG size


