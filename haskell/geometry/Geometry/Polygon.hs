{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}


module Geometry.Polygon
  (
    PiecewiseLinear (..)
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
  , Plot (..), plotManifold
  )
where

import Data.Complex
import Data.Foldable
import Data.List.Extra (minimumOn)
import Data.Monoid
import Data.Maybe
import Data.Fixed

import Geometry.Base
import Geometry.Point
import Geometry.Line

-- | A class for polylines and polygons.
class (Trans p, Manifold CN p) => PiecewiseLinear p where
  {-# MINIMAL vertices #-}
  -- | A list of polyline vertices.
  vertices :: p -> [CN]
  
  -- | A representation of the instance as a polyline.
  asPolyline :: p -> Polyline
  asPolyline = Polyline . vertices
  
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

instance PiecewiseLinear p => PiecewiseLinear (Maybe p) where
  vertices = maybe mempty vertices

-- | A predicate. Returns `True` if any of polyline's segment has zero length.
isDegenerate :: PiecewiseLinear p => p -> Bool
isDegenerate = any isZero . segments 

interpolation :: PiecewiseLinear p => p -> Double -> Maybe CN
interpolation p x = param' <$> find interval tbl
  where
    interval ((a, b), _) = a <= x && x <= b
    param' ((a, b), s) = s @-> ((x - a)/(b-a))
    tbl = zip (zip ds (tail ds)) $ segments p
    ds = scanl (+) 0 $ unit <$> segments p

-- | Instantiates as a single segment polyline.
instance PiecewiseLinear Segment where
  vertices s = let (p1,p2) = refPoints s in [p1,p2]

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

instance PiecewiseLinear Polyline where
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


instance Manifold CN Polyline where
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


instance Curve CN Polyline where
  tangent p t =  asCmp $ (p @-> (t + dt)) - (p @-> (t - dt))
    where dt = 1e-5
  normal p t = tangent p t # rotate (-90)

 
instance Figure Polyline where
  isTrivial p = length (vertices p) < 2
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

instance PiecewiseLinear Polygon where
  asPolyline p = Polyline $ take (length vs + 1) (cycle vs)
    where vs = vertices p
  vertices (Polygon vs) = vs
  segments = segments . asPolyline

  vertex p i = vs !! (i `mod` n)
    where vs = vertices (asPolyline p)
          n = verticesNumber p

  side p i = segments p !! (i `mod` n)
    where n = verticesNumber (asPolyline p)


instance Show Polygon where
  show p = concat ["<Polygon ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"


instance Manifold CN Polygon where
  param p t = fromJust $ interpolation (asPolyline p) $ (t `mod'` 1) * unit p
  project = project . asPolyline
  isContaining = isContaining . asPolyline
  unit = unit . asPolyline

instance Curve CN Polygon where
  tangent p t =  asCmp $ (p @-> (t + dt)) - (p @-> (t - dt))
    where dt = 1e-5
  normal p t = tangent p t # rotate (-90)
  
instance ClosedCurve CN Polygon where
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
           , Manifold CN
           , Curve CN
           , ClosedCurve CN
           , Trans
           , Eq
           , PiecewiseLinear
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
           , Manifold CN
           , Curve CN
           , ClosedCurve CN
           , Trans
           , Eq
           , PiecewiseLinear
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

------------------------------------------------------------

newtype Plot a = Plot { plotFn :: Double -> a }
  deriving Functor

instance Show (Plot a) where
  show _ = "<Plot>"

instance (AlmostEq a, Affine a, Trans a)=> Eq (Plot a) where
  p1 == p2 = plotManifold p1 == plotManifold p2

instance (AlmostEq a, Affine a, Trans a) => Trans (Plot a) where
  transform = fmap . transform

instance PiecewiseLinear (Plot CN) where
  vertices = vertices . plotManifold
  asPolyline = plotManifold

instance (AlmostEq a, Affine a, Trans a) => Curve a (Plot a) where
  tangent p t = asCmp $ cmp (p @-> (t + dt)) - cmp (p @-> (t - dt))
    where dt = 1e-5

instance (AlmostEq a, Affine a, Trans a) => Figure (Plot a) where
  refPoint = cmp . start
  box = box . plotManifold
  isTrivial = isTrivial . plotManifold

instance (AlmostEq a, Affine a, Trans a) => Manifold a (Plot a) where
  bounds _ = [0,1]
  param = plotFn
  project p pt = findZero (\x -> (azimuth (p @-> x) pt) `dot` tangent p x) x0
    where x0 = project pl (cmp pt)
          pl = plotManifold p
          p0 = pl @-> x0
  isContaining p = isContaining (plotManifold p) . cmp
  unit = unit . plotManifold

findZero f x = go x (x+dx) (f x) (f $ x + dx) 
  where go x1 x2 y1 y2 | abs (x2 - x1) < 1e-12 = x2
                       | otherwise = let x = (x1*y2 - x2*y1)/(y2-y1)
                                     in go x2 x y2 (f x)
        dx = 1e-5

plotManifold :: (Affine a, Manifold a m) => m -> Polyline
plotManifold m = Polyline pts
  where
    mf @->. x = cmp (mf @-> x) 
    pts = clean $ [m @->. 0] <> tree 0 1 <> [m @->. 1]
    tree a b | xa `distance` xb < 1e-4 = [xc]
             | abs (azimuth xa xc - azimuth xc xb) < asDeg 2 = [xc]
             | otherwise = tree a c <> tree c b
          where
            c = (a + b) / 2
            xa = m @->. a
            xb = m @->. b
            xc = m @->. c
    clean (x:y:z:t) | Segment (x,z) `isContaining` y = clean (x:z:t)
                    | otherwise = x : clean (y:z:t)
    clean xs = xs
    
