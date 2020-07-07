{-# language UndecidableInstances #-}

module TrianglesSpec where

import Test.Hspec
import Test.QuickCheck hiding (scale)

import Data.Maybe (isNothing, isJust)
import Data.List (tails)

import Geometry
import Geometry.Testing

--------------------------------------------------------------------------------

type PosReal = Positive Double

pairwise :: (a1 -> a1 -> a2) -> [a1] -> [a2]
pairwise f xs = concat . zipWith (fmap . f) xs $ tail <$> tails xs

coinside :: Metric a => [a] -> Bool
coinside = all (~= 0) . pairwise dist

isTangentToCircle :: Linear l => Circle -> l -> Bool
isTangentToCircle c l =
  let p = center c # projectOn l
  in distanceTo (center c) l ~= radius c &&
     0 ~= abs (l `cross` tangent c (p ->@ c))

allIntersections :: (Curve c, Intersections c c) => [c] -> [Cmp]
allIntersections = concat . pairwise intersections

--------------------------------------------------------------------------------

prop_anglesSum :: Triangle -> Bool
prop_anglesSum t = sum (vertexAngles t) ~= 180

prop_triangleInequality :: PosReal -> PosReal -> PosReal -> Property
prop_triangleInequality (Positive a) (Positive b) (Positive c) =
  (ineq <==> isJust (triangle3s a b c)) .||.
  (not ineq <==> isNothing (triangle3s a b c))
  where ineq = a + b >= c && a + c >= b && b + c >= a

prop_pytharogas1 :: RightTriangle -> Bool
prop_pytharogas1 t =
  let (a, b) = catets t
      c = hypotenuse t
  in unit c**2 ~= unit a**2 + unit b**2

prop_pytharogas2 :: PosReal -> PosReal -> Bool
prop_pytharogas2 (Positive a) (Positive b) =
  isRightTriangle . triangle3s a b $ sqrt (a*a + b*b)

--------------------------------------------------------------------------------

bisectrisses :: Triangle -> [Ray]
bisectrisses t = bisectrisse . (`vertexAngle` t) <$> [0,1,2]

prop_bisectrisses1 :: NonDegenerate Triangle -> Bool
prop_bisectrisses1 (NonDegenerate t) =
  coinside . allIntersections $ bisectrisses t 

prop_bisectrisses2 :: NonDegenerate Triangle -> Bool
prop_bisectrisses2 (NonDegenerate t) =
  let ps = allIntersections $ bisectrisses t
  in coinside [ p `distanceTo` s | p <- ps, s <- segments t ]
                   
prop_bisectrisses3 :: NonDegenerate Triangle -> Bool
prop_bisectrisses3 (NonDegenerate t) =
  let ps = allIntersections $ bisectrisses t
      p = sum ps / 3
  in and [ s # isTangentToCircle (circle' r p)
         | s <- segments t
         , let r = p `distanceTo` s]

--------------------------------------------------------------------------------

medians :: Triangle -> [Segment]
medians t = (\i -> median i (i+1) t) <$> [0,1,2]

centroid p = sum (vertices p) / fromIntegral (verticesNumber p)

prop_medians1 :: NonDegenerate Triangle -> Bool
prop_medians1 (NonDegenerate t) =
  coinside . allIntersections $ medians t  

prop_medians2 :: NonDegenerate Triangle -> Bool
prop_medians2 (NonDegenerate t) =
  all (~= c) . allIntersections $ medians t
  where c = centroid t

prop_medians3 :: NonDegenerate Triangle -> Bool
prop_medians3 (NonDegenerate t) = and $ pairwise division $ medians t
  where
    division m1 m2 = and [ c ->@ m ~= 2/3
                         | c <- intersections m1 m2
                         , m <- [m1, m2] ]

prop_Apollonius :: NonDegenerate Triangle -> Bool
prop_Apollonius (NonDegenerate t) =
  and [ unit (median i (i+1) t) ~= sqrt (2*b**2 + 2*c**2 - a**2) / 2
      | i <- [0,1,2]
      , let a = unit $ side (i + 1) t
            b = unit $ side i t
            c = unit $ side (i - 1) t ]

--------------------------------------------------------------------------------

altitudes :: Triangle -> [Segment]
altitudes t = (\i -> altitude i (i+1) t) <$> [0,1,2]

prop_altitudes1 :: NonDegenerate Triangle -> Bool
prop_altitudes1 (NonDegenerate t) =
  all (~= orthocenter t) . allIntersections $ altitudes t  

orthocenter :: Triangle -> Cmp
orthocenter t = head $ intersections h1 h2
  where h1 = t # altitude 0 1 # asLine
        h2 = t # altitude 1 2 # asLine

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Triangles:" $ do
    it "The triangle inequality."
      $ property prop_triangleInequality
    it "Sum of angles in a triangle is equal to 180."
      $ property prop_anglesSum
    it "The right thriangle is pithagorean."
      $ property prop_pytharogas1
    it "The pithagorean triangle is right."
      $ property prop_pytharogas2
    it "All bisectrisses intersect at one point."
      $ property prop_bisectrisses1
    it "Bisectrisses intersect at a center of the inscribed circle."
      $ property $ prop_bisectrisses2 .&&. prop_bisectrisses3
    it "All medians intersect at one point."
      $ property prop_medians1
    it "All medians intersect at the triangle's centroid."
      $ property prop_medians2
    it "Centroid divides medians in 1:2 ratio."
      $ property prop_medians3
    it "Apollonius' theorem."
      $ property prop_Apollonius
    it "All altitudes intersect at one point (orthocenter)."
      $ property prop_altitudes1

