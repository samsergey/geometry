{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TupleSections #-}

module Polygon
  (
    IsPoly (..)
  , isDegenerate
  , Polyline (..)
  , mkPolyline, trivialPolyline
  , Polygon (..)
  , mkPolygon, trivialPolygon, closePolyline
  , Triangle (..)
  , mkTriangle
  , Rectangle (..)
  , mkRectangle
  , boxRectangle
  )
where

import Data.Complex
import Data.Foldable
import Data.List.Extra (minimumOn)
import Data.Monoid
import Data.Maybe
import Data.Fixed

import Base
import Point
import Line

-- | A class for polylines and polygons.
-- Within instances of `IsPoly` class there are affine points and segments.
--
-- >>> asPolyline (1 :: CN)
-- <Polyline (1.0,0.0)>
--
-- >>> isDegenerate $ asPolyline (1 :: CN)
-- True
--
-- >>> asPolyline $ Segment (0:+2, 3:+4)
-- <Polyline (0.0,2.0) (3.0,4.0)>
--
-- >>> asPolyline (Segment (0:+2, 3:+4)) <> asPolyline (1 :: CN)
-- <Polyline (0.0,2.0) (3.0,4.0) (1.0,0.0)>
-- 
class IsPoly p where
  {-# MINIMAL vertices, asPolyline #-}
  -- | A list of polyline vertices.
  vertices :: p -> [CN]
  -- | A representation of the instance as a polyline.
  asPolyline :: p -> Polyline
  
  -- | The number of vertices of a polyline.
  verticesNumber :: p -> Int
  verticesNumber p = length (vertices p)

  -- | The list of segments of a polyline.
  segments :: p -> [Segment]
  segments p = case vertices p of
    [] -> []
    [x] -> [Segment (x,x)]
    vs -> Segment <$> zip vs (tail vs)

  -- | The indexed selector for vertices of a polyline.
  vertex :: p -> Int -> CN
  vertex p i = vs !! j
    where vs = vertices p
          n = verticesNumber p
          j = (0 `max` i) `min` (n - 1)

  -- | The indexed selector for sides of a polyline.
  side :: p -> Int -> Segment
  side p i = segments p !! j
    where j = (0 `max` i) `min` (n - 1)
          n = verticesNumber p         

-- | A predicate. Returns `True` if any of polyline's segment has zero length.
isDegenerate :: IsPoly p => p -> Bool
isDegenerate = any isZero . segments 

interpolation :: IsPoly p => p -> Double -> Maybe CN
interpolation p x = param' <$> find interval tbl
  where
    interval ((a, b), _) = a ~<= x && x ~<= b
    param' ((a, b), s) = s @-> ((x - a)/(b-a))
    tbl = zip (zip ds (tail ds)) $ segments p
    ds = scanl (+) 0 $ unit <$> segments p

-- | Instantiates as a single segment polyline.
instance IsPoly Segment where
  vertices s = let (p1,p2) = refPoints s in [p1,p2]
  asPolyline = Polyline . vertices

-- | Instantiates as a single point polyline.
instance IsPoly CN where
  vertices x = [x]
  asPolyline = Polyline . vertices

-- | Instantiates as a single point polyline.
instance IsPoly XY where
  vertices x = [cmp x]
  asPolyline = Polyline . vertices

-- | Instantiates as a single point polyline.
instance IsPoly Point where
  vertices x = [cmp x]
  asPolyline = Polyline . vertices

------------------------------------------------------------

-- | Representation of a polygonal chain as a list of vertices.
-- Vector representation of a polyline is given by the first segment of a polyline.
-- It is used for alignment or creation of polylines.
--
-- >>> cmp $ mkTriangle [0, 1:+0, 0:+1]
-- 1.0 :+ 0.0
--
-- >>> asCmp (1 :+ 2) :: Triangle
-- <Triangle (0.0,0.0) (1.0,2.0) (-1.2320508075688772,1.8660254037844393)>
--
-- >>> asCmp (1) :: Rectangle
-- <Rectangle (0.0,0.0) (1.0,0.0) (1.0,1.0) (0.0,1.0)>
--
newtype Polyline = Polyline [CN]

instance IsPoly Polyline where
  vertices (Polyline vs) = vs
  asPolyline = id

-- | Creates an empty polyline.
trivialPolyline :: Polyline
trivialPolyline = mempty

-- | The main polyline constructor.
mkPolyline :: Affine a => [a] -> Polyline
mkPolyline [] = Polyline [0, 0]
mkPolyline [x] = Polyline [cmp x, cmp x]
mkPolyline vs = Polyline $ cmp <$> vs

instance Show Polyline where
  show p = concat ["<Polyline ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"


instance Eq Polyline where
  p1 == p2 = vertices p1 ~== vertices p2

-- | Addition of polylines, excluding zero segments.
instance Semigroup Polyline where
  p1 <> p2 = Polyline $ vs
    where
      vs | null v1 = v2
         | null v2 = v1
         | otherwise = v1 <> m v2
        where
          v1 = vertices p1
          v2 = vertices p2
          m = if last v1 == head v2 then tail else id

instance Monoid Polyline where
  mempty = Polyline []


instance Affine Polyline where
  cmp p = case segments p of
            [] -> 0
            (x:_) -> cmp x
  asCmp x = mkPolyline [0, x]


instance Trans Polyline where
  transform t p = Polyline $ transform t <$> vertices p


instance Manifold Polyline where
  param p t | t < 0 = param (asLine (side p 0)) t
            | t > 1 = param (asLine (side p (verticesNumber p - 1))) t
            | otherwise = fromJust $  interpolation p (t * unit p) 

  project p pt = (x0 + (project s pt * unit s)) / unit p
    where
      ss = segments p
      ds = scanl (+) 0 $ unit <$> ss
      (x0, s) = minimumOn (\(_,s) -> distanceTo pt s) $ zip ds ss
      x = (x0 + (project s pt * unit s)) / unit p

  isContaining p x = any (`isContaining` x) (segments p)
  unit p = sum $ unit <$> segments p


instance Curve Polyline where
  tangent p t =  (p @-> (t + dt)) `azimuth` (p @-> (t - dt))
    where dt = 1e-5

 
instance Figure Polyline where
  isTrivial p = length (vertices p) < 2
  isSimilar p1 p2 = p1 == p2
  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0
  box p = foldMap (box . mkPoint) (vertices p)


------------------------------------------------------------

-- | Representation of a closed polygon as a list of vertices.
newtype Polygon = Polygon [CN]
  deriving ( Eq
           , Trans
           , Affine
           , Curve
           , Figure
           ) via Polyline

-- | Creates an empty polyline.
trivialPolygon :: Polygon
trivialPolygon = Polygon []

-- | The main polyline constructor.
mkPolygon :: Affine a => [a] -> Polygon
mkPolygon = Polygon . fmap cmp

-- | Turns a polyline to a polygon with the same vertices.
closePolyline :: Polyline -> Polygon
closePolyline p = Polygon $
              if last vs == head vs then (init vs) else vs
  where vs = vertices p

instance IsPoly Polygon where
  asPolyline p = Polyline $ take (length vs + 1) (cycle vs)
    where vs = vertices p
  vertices (Polygon vs) = vs
  vertex = vertex . asPolyline
  segments = segments . asPolyline
  side = side . asPolyline


instance Show Polygon where
  show p = concat ["<Polygon ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"


instance Manifold Polygon where
  param p t = fromJust $ interpolation p $ (t `mod'` 1) * unit p
  project = project . asPolyline
  isContaining = isContaining . asPolyline
  unit = unit . asPolyline

  
instance ClosedCurve Polygon where
  location p pt = case foldMap go (segments p') of
                    (Any True, _) -> OnCurve
                    (_, Sum n) | odd n -> Inside
                    _ -> Outside
    where
      p' = p # translate' (negate (cmp pt))
      go s | y0 * y1 == 0       = (Any True, mempty)
           | y0 == y1           = mempty
           | x == 0             = (Any True, mempty)
           | x > 0 && y0*y1 < 0 = (mempty, Sum 1)
           | otherwise          = mempty
        where
          (x0:+y0, x1:+y1) = refPoints s 
          x = (x0*y1-x1*y0)/(y1-y0)       

------------------------------------------------------------

-- | Representation of a triangle as a list of vertices.
newtype Triangle = Triangle [CN]
  deriving ( Figure
           , Manifold
           , Curve
           , ClosedCurve
           , Trans
           , Eq
           , IsPoly
           ) via Polygon

-- | The main triangle constructor.
mkTriangle :: Affine a => [a] -> Triangle
mkTriangle = Triangle . fmap cmp

instance Show Triangle where
  show t = concat ["<Triangle ", ss, ">"]
    where ss = unwords $ show . coord <$> vertices t

instance Affine Triangle where
  cmp = cmp . asPolyline
  asCmp = mkTriangle . scanl (+) 0 . take 2 . iterate (rotate 120)

------------------------------------------------------------

-- | Representation of a rectangle as a list of vertices.
newtype Rectangle = Rectangle [CN]
  deriving ( Figure
           , Manifold
           , Curve
           , ClosedCurve
           , Trans
           , Eq
           , IsPoly
           ) via Polygon

-- | The main rectangle constructor, uses two side lengths and guarantees
-- that polygon will have right angles.
mkRectangle :: Double -> Double -> Rectangle
mkRectangle a b = (asCmp 1 :: Rectangle) # scaleX a # scaleY b

instance Show Rectangle where
  show t = concat ["<Rectangle ", ss, ">"]
    where ss = unwords $ show . coord <$> vertices t

instance Affine Rectangle where
  cmp = cmp . asPolyline
  asCmp = Rectangle . scanl (+) 0 . take 3 . iterate (rotate 90)

-- | Returns a figures' box as a rectangle.
boxRectangle f = Rectangle [ p4, p3, p2, p1 ]
  where ((p4,p3),(p1,p2)) = corner f
