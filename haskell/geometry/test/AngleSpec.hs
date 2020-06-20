import Test.Hspec
import Test.QuickCheck hiding (scale)
import Test.Invariant

import Data.Complex
import Data.Fixed (mod')

import Geometry
import Geometry.Testing

main :: IO ()
main = hspec $ 
  describe "Angle" $ do
    describe "Affinity" $ do
      it "1" $ asCmp 1 == Angle 0 0 0
      it "2" $ property $ \x -> asCmp x == Angle 0 (asCmp x) (asCmp x)
      it "3" $ property $ \x -> cmp (Angle 0 0 x) == 1
      it "4" $ cmp (Angle 0 30 90) == cmp (asDeg 30)
      it "5" $ anAngle 30 # along 45 ~== Angle 0 45 75
      it "6" $ anAngle 30 # along 180 ~== Angle 0 180 210
      it "7" $ anAngle 30 # along 345 ~== Angle 0 345 15
      it "8" $ anAngle 30 # on (Segment (1, 2:+1)) 0 ~== Angle 1 45 75
      it "9" $ anAngle (-30) ~== Angle (0.0 :+ 0.0) 0 330

    describe "Manifold properties" $ do
      it "1" $ Angle 0 0 30 @-> 0 == 0
      it "2" $ Angle 0 0 30 @-> 1 == 30
      it "3" $ Angle 0 0 30 @-> 0.5 == 15
      it "4" $ Angle 0 0 30 @-> 2 == 60
      it "5" $ Angle 0 0 60 @-> 2 == 120
      it "7" $ Angle 0 30 120 @-> 0 == 30
      it "8" $ Angle 0 30 120 @-> 1 == 120
      it "9" $ Angle 0 0 358 @-> 0.5 == 179
      it "10" $ Angle 0 (-2) 0 @-> 0.5 == -1
      it "11" $ 0 ->@ Angle 0 0 30 == 0
      it "12" $ 45 ->@ Angle 0 0 30 == 1.5

    describe "Supplementary angles" $ do
      it "1" $ supplementary (Angle 0 30 45) ~== Angle 0 45 210
      it "2" $ supplementary (Angle 0 0 120) ~== Angle 0 120 180
      it "3" $ supplementary (Angle 0 10 200) ~== Angle 0 200 190
      it "4" $ property $ \(AnyAngle a) ->
        angleValue a + angleValue (supplementary a) ~== 180
