import Test.Hspec
import Test.Hspec.SmallCheck
import Test.Invariant

import Data.Complex

import Base


main :: IO ()
main = hspec $ do
  describe "Dir" $ do
    describe "equality" $ do
      it "1" $ do Ang 0 == Ang 360
      it "2" $ do Ang 10 == Ang 370
      it "3" $ do 45 == Vec (1 :+ 1)
      it "4" $ do 90 == Vec (0 :+ 1)
      it "5" $ do 90 == Ang (90 + 1e-12)
      it "6" $ do Vec (2 :+ 3) == Vec (4 :+ 6)

    describe "inequality" $ do
      it "1" $ do Ang 0 <= Ang 360
      it "2" $ do Ang 0 < Ang 360.0001
      it "3" $ do property $ \a -> 0 <= toTurns a && toTurns a <= 0.91
      
    describe "isomorphism" $ do
      it "1" $ do property $ toAng `inverts` toVec
      it "2" $ do property $ toVec `inverts` toAng

    describe "radians" $ do
      it "1" $ do toRad 10 == toRad 370
      it "2" $ do toRad (-10) == toRad 350
