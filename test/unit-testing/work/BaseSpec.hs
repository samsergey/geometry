{-# language TypeApplications #-}

import Test.Hspec
import Test.QuickCheck hiding (scale)
import Test.Invariant

import Data.Complex
import Data.Fixed (mod')
import Data.Semigroup

import Base
import Testing
import Line

main :: IO ()
main = hspec $ do
  describe "Angular" $ do
    describe "equality" $ do
      it "1" $ asDeg 0 == asDeg 360
      it "2" $ asDeg 10 == asDeg 370
      it "3" $ 45 == asCmp @Angular (1 :+ 1)
      it "4" $ 90 == asCmp @Angular (0 :+ 1)
      it "5" $ 90 == asDeg (90 + 1e-12)
      it "6" $ asCmp @Angular (2 :+ 3) == asCmp @Angular (4 :+ 6)

    describe "inequality" $ do
      it "1" $ asDeg 0 <= asDeg 360
      it "2" $ asDeg 0 < asDeg 360.0001
      it "3" $ property $ \a -> 0 <= turns a && turns a <= 1
      
    describe "isomorphism" $ do
      it "asRad" $ property $ \x -> (rad . asRad) x ~== x `mod'` (2*pi)
      it "asDeg" $ property $ \x -> (deg . asDeg) x ~== x `mod'` 360
      it "asTurns" $ property $ \x -> (turns . asTurns) x ~== x `mod'` 1

    describe "radians" $ do
      it "1" $ rad 10 ~== rad 370
      it "2" $ rad (-10) ~== rad 350

  describe "Affine" $ do
    describe "isomorphism" $ do
      it "1" $ property $ cmp `inverts` coord 
      it "2" $ property $ coord `inverts` cmp 
  
    describe "dot" $ do
      it "pair" $ dot @XY @XY (1, 2) (3, 6) == 15
      it "complex" $ dot @CN @CN (1 :+ 2) (3 :+ 6) == 15
  
    describe "cross" $ do
      it "pair" $ cross @XY @XY (1, 2) (3, 6) == 0
      it "complex" $ cross @CN @CN (1 :+ 2) (3 :+ 6) == 0
    
    describe "norm" $ do
      it "pair" $ norm @XY (3, 4) == 5
      it "complex" $ norm @CN (3 :+ 4) == 5

    describe "normalize" $ do
      it "pair" $ norm (normalize @XY (3, 4)) == 1
      it "complex" $ norm (normalize @CN (3 :+ 4)) == 1
      it "trivial" $ normalize @CN 0 == 0

  describe "Transformations" $ do
    describe "translate" $ do
      it "1" $ translate' @XY @XY (10, 20) (3, 4)  == (13, 24)
      it "2" $ translate' @XY @CN 10 (3, 4)  == (13, 4)
      it "3" $ translate' @CN @XY (3, 4) 10  == 13 :+ 4

    it "scale" $ scale @XY 10 (3, 4)  == (30, 40)
    it "scaleX" $ scaleX @XY 10 (3, 4) ~== (30, 4)
    it "scaleY" $ scaleY @XY 10 (3, 4) ~== (3, 40)

    describe "scaleAt" $ do
      it "1" $ scaleAt' @XY @XY (3, 4) 10 (3, 4)  == (3, 4)
      it "2" $ scaleAt' @XY @XY (0, 0) 10 (3, 4)  == (30, 40)
      it "3" $ scaleAt' @XY @XY (1, 1) 10 (3, 4)  == (21, 31)
      it "4" $ scaleAt' @XY @XY (1, 1) 10 (0, 0)  == (-9, -9)

    describe "rotate" $ do
      it "1" $ rotate @XY 0 (3, 4)  == (3, 4)
      it "2" $ rotate @XY 90 (3, 4)  ~== (-4, 3)
      it "3" $ rotate @XY 360 (3, 4)  == (3, 4)
      it "4" $ rotate @XY (asCmp (0 :+ 1)) (3, 4) ~== (-4, 3)

    describe "rotateAt" $ do
      it "1" $ rotateAt' @XY @XY (0, 0) 0 (3, 4) == (3, 4)
      it "2" $ rotateAt' @XY @XY (3, 4) 90 (3, 4) == (3, 4)
      it "3" $ rotateAt' @XY @XY (1, 0) 90 (0, 0) ~== (1, -1)
      it "4" $ rotateAt' @XY @XY (1, 1) 180 (0, 0) ~== (2, 2)

    describe "orientation" $ do
      it "1" $ property $ \a -> transformOrientation (rotateT a) == 1
      it "2" $ property $ \v -> transformOrientation (translateT v) == 1
      it "3" $ property $ \sx sy ->
        transformOrientation (scaleT sx sy) == (signum sx) * (signum sy)
      it "4" $ property $ \a ->
        transformOrientation (reflectT a) == -1

  describe "Box" $ do
    let l = Segment (1:+2, 4:+6)
    it "0" $ box l == ((1,2),(4,6))
    it "1" $ (left.lower.corner $ l) == 1:+2
    it "2" $ (right.lower.corner $ l) == 4:+2
    it "3" $ (left.upper.corner $ l) == 1:+6
    it "4" $ (right.upper.corner $ l) == 4:+6
    it "5" $ figureHeight l == 4
    it "6" $ figureWidth l == 3
