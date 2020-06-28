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

newtype RightTriangle = RightTriangle Triangle
  deriving (Show
           , Affine
           , Trans
           , Figure
           , Manifold
           , Curve
           , ClosedCurve
           , Polygonal
           , PiecewiseLinear)

mkRightTriangle :: Direction -> RightTriangle
mkRightTriangle = RightTriangle . triangle2a 90  . (`mod'` 90)

isRight :: PiecewiseLinear p => p -> Bool
isRight t = any (90 ~==) $ vertexAngles t

triangle3s a b c = case intersections c1 c2 of
                     [] -> Nothing
                     p:_ -> Just $ Triangle [0, a :+ 0, p]
  where c1 = aCircle # scale b
        c2 = aCircle # scale c # at (a, 0)

instance Arbitrary RightTriangle where
  arbitrary = (`suchThat` isNondegenerate) $ do
    a <- asDeg <$> choose (0, 90)
    m <- arbitrary
    Positive s <- arbitrary
    pure $ mkRightTriangle a # scale s # appMotion m
  shrink (RightTriangle t) = RightTriangle <$> shrink t

hypotenuse (RightTriangle t) = side 1 t
catets (RightTriangle t) = (side 0 t, side 2 t)

prop_Pytharogas_1 :: RightTriangle -> Bool
prop_Pytharogas_1 t =
  let (a, b) = catets t
      c = hypotenuse t
  in unit c**2 ~== unit a**2 + unit b**2

prop_Pytharogas_2 :: Double -> Double -> Bool
prop_Pytharogas_2 a b = isRight $ triangle3s a b $ sqrt (a*a + b*b)

main :: IO ()
main = hspec $
  describe "Pithagoras" $ do
    it "1" $ property prop_Pytharogas_1
    it "2" $ property prop_Pytharogas_2
     

