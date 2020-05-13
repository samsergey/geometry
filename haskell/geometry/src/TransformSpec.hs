{-# Language TypeApplications #-}
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.Invariant

import Data.Complex
import Data.AEq

import Base
import Transform

main :: IO ()
main = hspec $ do
  describe "Pos" $ do
    describe "isomorphism" $ do
      it "1" $ do property $ pos `inverts` coord 
      it "2" $ do property $ coord `inverts` pos 

    describe "dot" $ do
      it "pair" $ do dot @XY (1, 2) (3, 6) == 15
      it "complex" $ do dot @CXY (1 :+ 2) (3 :+ 6) == 15

    describe "cross" $ do
      it "pair" $ do cross @XY (1, 2) (3, 6) == 0
      it "complex" $ do cross @CXY (1 :+ 2) (3 :+ 6) == 0

    describe "norm" $ do
      it "pair" $ do norm @XY (3, 4) == 5
      it "complex" $ do norm @CXY (3 :+ 4) == 5

    describe "normalize" $ do
      it "pair" $ do norm (normalize @XY (3, 4)) == 1
      it "complex" $ do norm (normalize @CXY (3 :+ 4)) == 1
      it "trivial" $ do normalize @CXY 0 == 0

  describe "Transformations" $ do
    describe "translate" $ do
      it "1" $ do translate @XY @XY (10, 20) (3, 4)  == (13, 24)
      it "2" $ do translate (10 :: CXY) ((3, 4) :: XY)  == ((13, 4) :: XY)
      it "3" $ do translate ((3, 4) :: XY) (10 :: CXY)  == (13 :+ 4 :: CXY)

    it "scale" $ do scale @XY 10 (3, 4)  == (30, 40)

    describe "scaleAt" $ do
      it "1" $ do scaleAt @XY @XY (3, 4) 10 (3, 4)  == (3, 4)
      it "2" $ do scaleAt @XY @XY (0, 0) 10 (3, 4)  == (30, 40)
      it "3" $ do scaleAt @XY @XY (1, 1) 10 (3, 4)  == (21, 31)
      it "4" $ do scaleAt @XY @XY (1, 1) 10 (0, 0)  == (-9, -9)

    describe "rotate" $ do
      it "1" $ do rotate @XY 0 (3, 4)  == (3, 4)
      it "2" $ do rotate @XY 90 (3, 4)  ~== (-4, 3)
      it "3" $ do rotate @XY 360 (3, 4)  == (3, 4)
      it "4" $ do rotate @XY (Vec (0 :+ 1)) (3, 4) ~== (-4, 3)

    describe "rotateAt" $ do
      it "1" $ do rotateAt @XY @XY (0, 0) 0 (3, 4) == (3, 4)
      it "2" $ do rotateAt @XY @XY (3, 4) 90 (3, 4) == (3, 4)
      it "3" $ do rotateAt @XY @XY (1, 0) 90 (0, 0) ~== (1, -1)
      it "4" $ do rotateAt @XY @XY (1, 1) 180 (0, 0) ~== (2, 2)
