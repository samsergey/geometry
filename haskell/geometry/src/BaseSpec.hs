import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex

import Base
import Testing


main :: IO ()
main = hspec $
  describe "Angular" $ do
    describe "equality" $ do
      it "1" $ Deg 0 == Deg 360
      it "2" $ Deg 10 == Deg 370
      it "3" $ 45 == Cmp (1 :+ 1)
      it "4" $ 90 == Cmp (0 :+ 1)
      it "5" $ 90 == Deg (90 + 1e-12)
      it "6" $ Cmp (2 :+ 3) == Cmp (4 :+ 6)

    describe "inequality" $ do
      it "1" $ Deg 0 <= Deg 360
      it "2" $ Deg 0 < Deg 360.0001
      it "3" $ property $ \a -> 0 <= toTurns a && toTurns a <= 1
      
    describe "isomorphism" $ do
      it "1" $ property $ deg `inverts` toCmp
      it "2" $ property $ toCmp `inverts` deg

    describe "radians" $ do
      it "1" $ rad 10 == rad 370
      it "2" $ rad (-10) == rad 350
