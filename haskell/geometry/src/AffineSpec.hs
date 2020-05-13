{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck hiding (scale)
import Test.Invariant

import Data.Complex

import Base
import Affine
import Testing

main :: IO ()
main = hspec $ do
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
