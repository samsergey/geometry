import Test.Hspec
import Test.QuickCheck hiding (scale)
import Test.Invariant

import Data.Complex

import Base
import Testing


main :: IO ()
main = hspec $ do
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
      it "3" $ property $ \a -> 0 <= turns a && turns a <= 1
      
    describe "isomorphism" $ do
      it "Rad 1" $ property $ (Rad . rad) `inverts` toRad
      it "Rad 2" $ property $ involutory toRad
      it "Rad 3" $ property $ idempotent toRad
      it "Deg 1" $ property $ (Deg . deg) `inverts` toDeg
      it "Deg 2" $ property $ involutory toDeg
      it "Deg 3" $ property $ idempotent toDeg
      it "Cmp 1" $ property $ (Cmp . cmp) `inverts` toCmp
      it "Cmp 2" $ property $ involutory toCmp
      it "Cmp 3" $ property $ idempotent toCmp
      it "Turns 1" $ property $ (Turns . turns) `inverts` toTurns
      it "Turns 2" $ property $ involutory toTurns
      it "Turns 3" $ property $ idempotent toTurns
      it "1" $ property $ (toTurns . toCmp) <=> toTurns
      it "2" $ property $ (toTurns . toCmp . toDeg) <=> toTurns
      it "3" $ property $ (toTurns . toCmp . toDeg . toRad) <=> toTurns

    describe "radians" $ do
      it "1" $ rad 10 == rad 370
      it "2" $ rad (-10) == rad 350

    describe "Affine" $ do
      describe "isomorphism" $ do
        it "1" $ property $ cmp `inverts` coord 
        it "2" $ property $ coord `inverts` cmp 
  
    describe "dot" $ do
      it "pair" $ dot @XY (1, 2) (3, 6) == 15
      it "complex" $ dot @CN (1 :+ 2) (3 :+ 6) == 15
  
    describe "cross" $ do
      it "pair" $ cross @XY (1, 2) (3, 6) == 0
      it "complex" $ cross @CN (1 :+ 2) (3 :+ 6) == 0
  
    describe "norm" $ do
      it "pair" $ norm @XY (3, 4) == 5
      it "complex" $ norm @CN (3 :+ 4) == 5

    describe "normalize" $ do
      it "pair" $ norm (normalize @XY (3, 4)) == 1
      it "complex" $ norm (normalize @CN (3 :+ 4)) == 1
      it "trivial" $ normalize @CN 0 == 0

  describe "Transformations" $ do
    describe "translate" $ do
      it "1" $ translate @XY @XY (10, 20) (3, 4)  == (13, 24)
      it "2" $ translate @XY @CN 10 (3, 4)  == (13, 4)
      it "3" $ translate @CN @XY (3, 4) 10  == 13 :+ 4

    it "scale" $ scale @XY 10 (3, 4)  == (30, 40)

    describe "scaleAt" $ do
      it "1" $ scaleAt @XY @XY (3, 4) 10 (3, 4)  == (3, 4)
      it "2" $ scaleAt @XY @XY (0, 0) 10 (3, 4)  == (30, 40)
      it "3" $ scaleAt @XY @XY (1, 1) 10 (3, 4)  == (21, 31)
      it "4" $ scaleAt @XY @XY (1, 1) 10 (0, 0)  == (-9, -9)

    describe "rotate" $ do
      it "1" $ rotate @XY 0 (3, 4)  == (3, 4)
      it "2" $ rotate @XY 90 (3, 4)  ~== (-4, 3)
      it "3" $ rotate @XY 360 (3, 4)  == (3, 4)
      it "4" $ rotate @XY (Cmp (0 :+ 1)) (3, 4) ~== (-4, 3)

    describe "rotateAt" $ do
      it "1" $ rotateAt @XY @XY (0, 0) 0 (3, 4) == (3, 4)
      it "2" $ rotateAt @XY @XY (3, 4) 90 (3, 4) == (3, 4)
      it "3" $ rotateAt @XY @XY (1, 0) 90 (0, 0) ~== (1, -1)
      it "4" $ rotateAt @XY @XY (1, 1) 180 (0, 0) ~== (2, 2)
