{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language DerivingVia #-}
{-# language DeriveFunctor #-}
{-# language StandaloneDeriving #-}
{-# language ConstraintKinds #-}
{-# language BangPatterns #-}

module Geometry.Plot
     ( APlot (..)
     , Plot (..), plot
     , ClosedPlot (..), closedPlot
     , plotManifold
     , intersectionsP
     ) where

import Control.Applicative
import Control.Monad

import Geometry.Base
import Geometry.Point
import Geometry.Line
import Geometry.Polygon
import Numeric.MathFunctions.Comparison
import qualified Numeric.RootFinding as RF

--------------------------------------------------------------------------------
-- | Class for objects representing plots.
class APlot p where
  {-# MINIMAL rmap #-}

  {- | Mapping on the input parameter of the plot.

     @ rmap f (plot p) =  plot (d . f) @
    -}
  rmap :: (Double -> Double) -> p -> p

  -- | Changes or sets the parameter range of the plot.
  range :: (Double, Double) -> p -> p
  range (a,b) = rmap $ \x -> a + x * (b - a)

instance APlot p => APlot (Maybe p) where
  rmap = fmap . rmap
  
--------------------------------------------------------------------------------  
{- | A manifold with explicit parameter function. Could be used for parametric plotting.

> let p1 = plot (\t -> (t, abs (sin t)))
>     p2 = p1 # range (0, 3)
>     p3 = p1 # range (0, 7)
> in p1 `above` p2 `above` p3
<< figs/plot.svg >>
-}
newtype Plot a = Plot (Double -> a, Polyline)
  deriving Functor 

-- | Smart constructor for a Plot
plot f = Plot (f, plotParam f)

instance Pnt a => APlot (Plot a) where
  rmap f p = plot $ param p . f

instance Show (Plot a) where
  show _ = "<Plot>"

instance Pnt a => Manifold (Plot a) where
  type Domain (Plot a) = a
  bounds = const Bound
  param (Plot (f, _)) = f
  project p@(Plot (f, _)) pt =
    let x0 = pt ->@ asPolyline p
        goal x = dot (azimuth (f x) pt) (tangent p x)
    in case secant goal x0 of
         Nothing -> let goal2 = dist2 pt . f 
                    in findMin goal2 x0
         Just x -> x
  paramMaybe p x | 0 <= x && x <= 1 = pure $ param p x
                   | otherwise = Nothing
  isContaining p pt = 0 ~<= x && x ~<= 1 && (p @-> x) `distance` pt ~<= 1e-10
    where x = pt ->@ p
    
  unit = unit . asPolyline
  
instance Pnt a => Eq (Plot a) where
  p1 == p2 = asPolyline p1 == asPolyline p2

instance Pnt a => Trans (Plot a) where
  transform t (Plot (f, p)) = Plot (transform t . f, transform t p)

instance Pnt a => PiecewiseLinear (Plot a) where
  asPolyline (Plot (_,!p)) = p

instance Pnt a => Curve (Plot a) where
  tangent p t = asCmp $ cmp (p @-> (t + dt)) - cmp (p @-> (t - dt))
    where dt = 1e-8

instance Pnt a => Figure (Plot a) where
  refPoint = left . lower . corner
  box = box . asPolyline
  isTrivial = isTrivial . asPolyline

--------------------------------------------------------------------------------

{- | A manifold with explicit parameter function. Could be used for parametric plotting.

> let flower t = scale (2 + sin (5*t)) (cos t, sin t)
>     p = closedPlot flower # range (0, 2*pi)
> in p <||> space 1 <||> p # scaleX 0.5 # rotate 30
<< figs/closedPlot.svg >>
-}
newtype ClosedPlot a = ClosedPlot (Double -> a, Polyline)
  deriving Functor

-- | Smart constructor for a ClosedPlot
closedPlot f = ClosedPlot (f, plotParam f)

instance Show (ClosedPlot a) where
  show _ = "<ClosedPlot>"

instance Pnt a => Manifold (ClosedPlot a) where
  type Domain (ClosedPlot a) = a
  bounds = const Unbound
  param (ClosedPlot (f, _)) = f
  project p pt = let x0 = pt ->@ asPolyline p
                     goal x = dot (azimuth (p @-> x) pt) (tangent p x) 
                 in case secant goal x0 of
                      Nothing -> let goal x = dist2 (p @-> x) pt 
                                 in findMin goal x0
                      Just x -> x
  isContaining p pt = (p @-> (pt ->@ p)) `distance` pt <= 1e-10
    
  unit = unit . asPolyline

instance Pnt a => PiecewiseLinear (ClosedPlot a) where
  asPolyline (ClosedPlot (_,!p)) = p

instance Polygonal (ClosedPlot Cmp) where

instance Pnt a => ClosedCurve (ClosedPlot a) where
  isEnclosing = isEnclosing . closePolyline . asPolyline

deriving via Plot a instance Pnt a => APlot (ClosedPlot a)
deriving via Plot a instance Pnt a => Eq (ClosedPlot a)
deriving via Plot a instance Pnt a => Trans (ClosedPlot a)
deriving via Plot a instance Pnt a => Figure (ClosedPlot a)
deriving via Plot a instance Pnt a => Curve (ClosedPlot a)

-- | Returns a polyline on an adaptive mesh for  a given smooth manifold.
plotManifold :: Manifold m => m -> Polyline
plotManifold = plotParam . param 

--------------------------------------------------------------------------------

plotParam :: Affine a => (Double -> a) -> Polyline
plotParam mf = Polyline pts
  where
    f = cmp . mf
    phi = (sqrt 5 - 1) / 2
    pts = clean $ [f 0] <> mconcat (zipWith tree xs (tail xs)) <> [f 1]
    xs = 0 : (reverse $ take 5 $ iterate (*phi) 1)
    tree a b | xa `distance` xb < 1e-3 = [xc]
             | abs (azimuth xa xc - azimuth xc xb) < asDeg 3 = [xc]
             | otherwise = tree a c <> tree c b
          where
            c = a + (b - a) * phi
            xa = f a
            xb = f b
            xc = f c
    clean (x:y:z:t) | Segment (x,z) `isContaining` y = clean (x:z:t)
                    | otherwise = x : clean (y:z:t)
    clean xs = xs

--------------------------------------------------------------------------------
intersectionsP m1 m2 = foldMap refine
  where
    dist a b = dist2 (cmp (m1 @-> a)) (cmp (m2 @-> b))
    refine x =
      case steepestDescent dist (x ->@ m1, x ->@ m2) of
        Nothing -> []
        Just (t1, t2) -> [asAffine (m1 @-> t1)]
        
limit :: (Metric b) => [b] -> Maybe b
limit [] = Nothing
limit xs = case res of
             [] -> Nothing
             ((_,y):_) -> Just y
  where res = dropWhile (\(x, y) -> dist x y > 1e-14) $
              take 100 $
              zip xs (tail xs)

fixpoint f = limit . iterate f

steepestDescent
  :: (Double -> Double -> Double)
     -> (Double, Double) -> Maybe (Double, Double)
steepestDescent f (x, y) = fixpoint step (x,y)
  where
    step (x, y) = let (dx, dy) = grad f x y
                      t = findMin (\t -> f (x+dx*t) (y+dy*t)) 0
                 in (x + dx*t, y + dy*t)

    grad f x y = ( (f (x + d) y - f (x - d) y)/(2*d)
                 , (f x (y + d) - f x (y - d))/(2*d) )
      where d = 1e-8


goldenMean :: (Double -> Double) -> Double -> Double -> Double
goldenMean f x1 x2 = go (x1, c, d, x2, 0)
  where
    phi = (sqrt 5 - 1)/2
    c = x2 - (x2 - x1)*phi
    d = x1 + (x2 - x1)*phi
    go (a, c, d, b, i) 
      | abs (d - c) <= 1e-14 || i > 50 = (d+c)/2 
      | f c < f d = go (a, d - (d - a)*phi, c, d, i+1)
      | otherwise  = go (c, d, c + (b - c)*phi, b, i+1)

findMin :: (Double -> Double) -> Double -> Double
findMin f x = go x x1 (3*x1 - 2*x)
  where
    dx = 1e-3
    x1 | f x > f (x + dx) = x + dx
       | otherwise = x - dx
    go x1 x2 x3 | f x2 < f x3 = goldenMean f x1 x3
                | otherwise = go x2 x3 (3*x3 - 2*x2)
    

------------------------------------------------------------

bisection f a b
  | f a * f b > 0       = empty
  | abs (a - b) < 1e-12 = pure c
  | otherwise           = bisection f a c <|> bisection f c b
  where c = (a + b) / 2

findRoot method f xs = msum $ zipWith (method f) xs (tail xs)

secant :: (Ord a, Fractional a) => (a -> a) -> a -> Maybe a
secant f x = go x (x+dx) (f x) (f $ x + dx) 100
  where go x1 x2 y1 y2 i
          | y2 == y1 || i <= 0 = Nothing
          | abs (x2 - x1) < 1e-14 = Just x2            
          | abs (x2 - x1) < 1e-14 * abs (x1 + x2) = Just x2            
          | otherwise = let x = (x1*y2 - x2*y1)/(y2-y1)
                        in go x2 x y2 (f x) (i-1)
        dx = 1e-5

--------------------------------------------------------------------------------

diff f x = [ (f (x+dx) - f (x-dx))/(2*dx) | dx <- iterate (/2) 1e-3 ]
