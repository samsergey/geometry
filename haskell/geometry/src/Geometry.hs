{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
module Geometry (
  -- * Reexports modules
    module Base
  , module Point
  , module Line
  , module Circle
  , module Polygon
  , module Decorations
  -- * Main interface
  , writeSVG, showSVG, (<+>), group
  -- * Constructors for affine geometric objects
  , aPoint, aLabel
  , aLine, aRay, aSegment
  , aCircle
  , aSquare, aRectangle, aTriangle
  -- * Constructors for exact objects
  , origin, oX, oY
  , point, pointOn
  , circle
  , line, ray, segment
  , parametricPoly, polarPoly, regularPoly
  -- * Modificators
  , at, along, through
  , perpendicularTo
  -- * General versions of conctructors and modifiers
  , point', line', ray', segment'
  , circle'
  , at', along', through',
  ) where

import Prelude hiding (writeFile)
import Data.Complex
import Data.Text.Lazy.IO (writeFile)

import Base
import Point
import Circle
import Line
import Polygon
import Decorations
import SVG
 
------------------------------------------------------------

origin :: Point
origin = mkPoint (0 :: CN)

oX = aLine
oY = oX # rotate 90

------------------------------------------------------------

point :: XY -> Point
point = point'

point' :: Affine a => a -> Point
point' p = mkPoint (cmp p)

pointOn :: Curve a => a -> Double -> Point
pointOn c t = mkPoint (c @-> t)

aPoint :: Point
aPoint = origin

aLabel :: String -> Decorated Label
aLabel s = mkLabel origin #: label s

------------------------------------------------------------

circle' :: Affine a => Double -> a -> Circle
circle' r p = mkCircleRC r (cmp p)

circle :: Double -> XY -> Circle
circle = circle'

aCircle :: Circle
aCircle = circle' 1 origin

------------------------------------------------------------

line' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line' p1 p2 = mkLine (cmp p1, cmp p2)

line :: XY -> XY -> Line
line = line'

segment' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
segment' p1 p2 = mkSegment (cmp p1, cmp p2)

segment :: XY -> XY -> Line
segment = segment'

ray' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
ray' p1 p2 = mkRay (cmp p1, cmp p2)

ray :: XY -> XY -> Line
ray = ray'

aSegment, aLine, aRay :: Line
aSegment = segment' origin ((1,0) :: XY)
aLine = aSegment `extendAs` Unbound
aRay = aSegment `extendAs` Semibound

------------------------------------------------------------
at' :: (Affine p, Figure a) => p -> a -> a
at' p fig = superpose (refPoint fig) p fig

at :: Figure a => XY -> a -> a
at = at'

along' :: (Figure f, Affine v, Affine f) => v -> f -> f
along' v l = rotateAt (refPoint l) (angle v - angle l) l

along :: (Figure a, Affine a) => Double -> a -> a
along d = along' (asDeg d)


-- | Turns and extends the line so that it passes through a given point.
through' :: (Affine p, Linear l) => p -> l -> l
through' p l = l
               # along' (azimuth p0 p)
               # scaleAt p0 (distance p0 p / unit l)
  where p0 = start l

-- | A coordinated version of `through'.
through :: Linear l => XY -> l -> l
through = through'

-- | Turns the line so that it becomes perpendicular to a given one, pointing towards it.
perpendicularTo :: Linear l => Line -> l -> l
perpendicularTo l2 l = l # along' d
  where
    s = start l
    s' = s # reflectAt l2
    d = if l2 `isContaining` s
        then normal l2 0
        else angle $ ray' s s'

------------------------------------------------------------

parametricPoly :: (Double -> XY) -> [Double] -> Polygon
parametricPoly f range =
  mkPolyline [ x :+ y | t <- range , let (x,y) = f t ]

polarPoly :: (Double -> Double) -> [Double] -> Polygon
polarPoly rho range =
  mkPolyline [ mkPolar (rho phi) phi | x <- range
                                     , let phi = 2*pi*x ]

regularPoly :: Int -> Polygon
regularPoly n' = rotate 90 $ closePoly $
                 polarPoly (const 1) [0,1/n..1-1/n]
  where n = fromIntegral n'

aSquare :: Polygon
aSquare = mkPolygon @XY [(0,0),(1,0),(1,1),(0,1)]

aRectangle :: Double -> Double -> Polygon
aRectangle a b = aSquare # scaleX a # scaleY b

aTriangle :: Polygon
aTriangle = mkPolygon @XY [ (0,0), (1,0)
                          , (cos (pi/3), sin (pi/3))]


writeSVG :: SVGable a => FilePath -> a -> IO ()
writeSVG name g = writeFile name $ showSVG g
