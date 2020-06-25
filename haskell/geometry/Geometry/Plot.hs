{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingVia #-}
{-# language DerivingStrategies #-}
{-# language StandaloneDeriving #-}

module Geometry.Plot
     ( APlot (..)
     , Plot, plot
     , ClosedPlot, closedPlot
     , plotManifold
     ) where

import Geometry.Base
import Geometry.Point
import Geometry.Line
import Geometry.Polygon

--------------------------------------------------------------------------------
-- | Class for objects representing plots.
class APlot p where
  {-# MINIMAL premap #-}
  {- | Mapping on the input parameter of the plot.

     @ premap f (plot p) =  plot (d . f) @
    -}
  premap :: (Double -> Double) -> p -> p

  {- | Changes or sets the parameter range of the plot.

<< figs/range.svg >>
    -}
  range :: (Double, Double) -> p -> p
  range (a,b) = premap $ \x -> a + x * (b - a)

instance APlot p => APlot (Maybe p) where
  premap f p = premap f <$> p
  
{- | A manifold with explicit parameter function. Could be used for parametric plotting.

> let p = closedPlot (\t -> (cos t, sin t)) # range (0, 2*pi)
> in p <||> (p # scaleX 0.5) <||> (p # scaleX 0.5 # rotate 30) <||> 
<< figs/plot.svg >>
-}
newtype Plot a = Plot (Double -> a, Polyline)

-- | Smart constructor for a Plot
plot f = Plot (f, plotParam f)

instance (Affine a, Trans a) => APlot (Plot a) where
  premap f p = plot $ (param p) . f

instance Show (Plot a) where
  show _ = "<Plot>"

instance (Affine a, Trans a) => Manifold (Plot a) where
  type Domain (Plot a) = a
  bounds = const Bound
  param (Plot (f, _)) = f
  project p pt = let x0 = project (asPolyline p) (cmp pt)
                 in findMin (\x -> (p @-> x) `distance` pt) x0
  isContaining p pt = 0 ~<= x && x ~<= 1 && (p @-> x) `distance` pt <= 1e-5
    where x = pt ->@ p
    
  unit = unit . asPolyline
  
instance (Affine a, Trans a)=> Eq (Plot a) where
  p1 == p2 = asPolyline p1 == asPolyline p2

instance (Affine a, Trans a) => Trans (Plot a) where
  transform t (Plot (f, p)) = Plot (transform t . f, transform t p)

instance (Affine a, Trans a) => PiecewiseLinear (Plot a) where
  asPolyline (Plot (_,p)) = p

instance (Affine a, Trans a) => Curve (Plot a) where
  tangent p t = asCmp $ cmp (p @-> (t + dt)) - cmp (p @-> (t - dt))
    where dt = 1e-5

instance (Affine a, Trans a) => Figure (Plot a) where
  refPoint = left . lower . corner
  box = box . asPolyline
  isTrivial = isTrivial . asPolyline

--------------------------------------------------------------------------------

newtype ClosedPlot a = ClosedPlot (Double -> a, Polyline)

-- | Smart constructor for a ClosedPlot
closedPlot f = Plot (f, plotParam f)

instance Show (ClosedPlot a) where
  show _ = "<ClosedPlot>"

instance (Affine a, Trans a) => Manifold (ClosedPlot a) where
  type Domain (ClosedPlot a) = a
  bounds = const Unbound
  param (ClosedPlot (f, _)) = f
  project p pt = let x0 = project (asPolyline p) (cmp pt)
                 in findMin (\x -> (p @-> x) `distance` pt) x0
  isContaining p pt = (p @-> (pt ->@ p)) `distance` pt <= 1e-5
    
  unit = unit . asPolyline

instance (Affine a, Trans a) => PiecewiseLinear (ClosedPlot a) where
  asPolyline (ClosedPlot (_,p)) = p

instance Polygonal (ClosedPlot Cmp) where

instance (Affine a, Trans a) => ClosedCurve (ClosedPlot a) where
  isEnclosing = isEnclosing . closePolyline . asPolyline

deriving via Plot a instance (Affine a, Trans a) => APlot (ClosedPlot a)
deriving via Plot a instance (Affine a, Trans a) => Eq (ClosedPlot a)
deriving via Plot a instance (Affine a, Trans a) => Trans (ClosedPlot a)
deriving via Plot a instance (Affine a, Trans a) => Figure (ClosedPlot a)
deriving via Plot a instance (Affine a, Trans a) => Curve (ClosedPlot a)

-- | Returns a polyline on an adaptive mesh for  a given smooth manifold.
plotManifold :: Manifold m => m -> Polyline
plotManifold = plotParam . param 

--------------------------------------------------------------------------------

plotParam :: Affine a => (Double -> a) -> Polyline
plotParam f = Polyline pts
  where
    mf @->. x = cmp (f x) 
    pts = clean $ [f @->. 0] <> tree 0 0.5 <> tree 0.5 1 <> [f @->. 1]
    tree a b | xa `distance` xb < 1e-4 = [xc]
             | abs (azimuth xa xc - azimuth xc xb) < asDeg 2 = [xc]
             | otherwise = tree a c <> tree c b
          where
            c = (a + b) / 2
            xa = f @->. a
            xb = f @->. b
            xc = f @->. c
    clean (x:y:z:t) | Segment (x,z) `isContaining` y = clean (x:z:t)
                    | otherwise = x : clean (y:z:t)
    clean xs = xs

--------------------------------------------------------------------------------

findZero f x = go x (x+dx) (f x) (f $ x + dx) 
  where go x1 x2 y1 y2 | abs (x2 - x1) < 1e-12 = x2
                       | otherwise = let x = (x1*y2 - x2*y1)/(y2-y1)
                                     in go x2 x y2 (f x)
        dx = 1e-5

goldenMean f x1 x2 = go (x1, c, d, x2, 0)
  where
    goldenMean = (sqrt 5 - 1)/2
    c = x2 - (x2 - x1)*goldenMean
    d = x1 + (x2 - x1)*goldenMean
    go (a, c, d, b, i) 
      | abs l < 1e-9 || i > 100 = (d+c)/2 -- quadraticMin f a b ((d+c)/2)
      | f c < f d = go (a, a + l, c, d, i+1)
      | otherwise = go (c, d, b - l, b, i+1)
      where l = d - c 

quadraticMin f x1 x2 x3 | True = x
                        | otherwise = quadraticMin f x2 x3 x
  where x | d /= 0 = (x1**2*(y3-y2)-x2**2*y3+x3**2*y2+(x2**2-x3^2)*y1)/d
          | otherwise = x1
        d = 2*(x1*(y3-y2)-x2*y3+x3*y2+(x2-x3)*y1)
        y1 = f x1
        y2 = f x2
        y3 = f x3

findMin f x = go x x1 (3*x1 - 2*x)
  where
    dx = 1e-3
    x1 | f x > f (x + dx) = x + dx
       | otherwise = x - dx
    go x1 x2 x3 | f x2 <= f x3 = goldenMean f x1 x3
                | otherwise = go x2 x3 (3*x3 - 2*x2)
    

