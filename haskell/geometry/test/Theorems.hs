{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}

import Test.Hspec
import Test.QuickCheck hiding (scale)
import Test.Invariant

import Data.Complex
import Data.Fixed (mod')
import Data.Maybe

import Geometry
import Geometry.Testing

prop_AnglesSum :: Triangle -> Bool
prop_AnglesSum t = sum (vertexAngles t) ~== 180

prop_TriangleIneq (Positive a) (Positive b) (Positive c) =
  (ineq <==> isJust (triangle3s a b c)) .||.
  (not ineq <==> isNothing (triangle3s a b c))
  where ineq = a + b >= c && a + c >= b && b + c >= a

prop_Pytharogas_1 :: RightTriangle -> Bool
prop_Pytharogas_1 t =
  let (a, b) = catets t
      c = hypotenuse t
  in unit c**2 ~== unit a**2 + unit b**2

prop_Pytharogas_2 (Positive a) (Positive b) =
  isRightTriangle $ triangle3s a b $ sqrt (a*a + b*b)

prop_Trig_bisectrisse :: NonDegenerate Triangle -> Bool
prop_Trig_bisectrisse (NonDegenerate t) = let
  [b1,b2,b3] = [t # vertexAngle i # bisectrisse | i <- [0,1,2] ]
  [p12] = intersections b1 b2
  [p13] = intersections b1 b3
  [p23] = intersections b2 b3
  in p12 ~== p13 && p12 ~== p23 && p13 ~== p23 
  

main :: IO ()
main = hspec $ do
  describe "Triangles" $ do
    it "The triangle inequality" $
      property prop_TriangleIneq
    it "Sum of angles in a triangle is equal to 180" $
      property prop_AnglesSum
  
  describe "Pithagoras" $ do
    it "1" $ property prop_Pytharogas_1
    it "2" $ property prop_Pytharogas_2

  describe "Bisectrisse" $ do
    it "1" $ property prop_Trig_bisectrisse


