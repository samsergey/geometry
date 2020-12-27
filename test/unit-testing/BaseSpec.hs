{-# language TypeApplications #-}

module BaseSpec where

import Data.Complex
import Data.Fixed (mod')
import Data.Semigroup
import Geometry
import Geometry.Testing
import Test.Hspec
import Test.Invariant
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = do
  describe "Direction" $ do
    describe "equality" $ do
      it "1" $ asDeg 0 == asDeg 360
      it "2" $ asDeg 10 == asDeg 370
      it "3" $ 45 == asCmp @Direction (1 :+ 1)
      it "4" $ 90 == asCmp @Direction (0 :+ 1)
      it "5" $ 90 == asDeg (90 + 1e-12)
      it "6" $ asCmp @Direction (2 :+ 3) == asCmp @Direction (4 :+ 6)

    describe "inequality" $ do
      it "1" $ asDeg 0 <= asDeg 360
      it "2" $ asDeg 0 < asDeg 360.0001
      it "3" $ property $ \a -> 0 <= turns a && turns a <= 1
      
    describe "isomorphism" $ do
      it "asRad" $ property $ \x -> (rad . asRad) x ~= x `mod'` (2*pi)
      it "asDeg" $ property $ \x -> (deg . asDeg) x ~= x `mod'` 360
      it "asTurns" $ property $ \x -> (turns . asTurns) x ~= x `mod'` 1

    describe "radians" $ do
      it "1" $ rad 10 ~= rad 370
      it "2" $ rad (-10) ~= rad 350

  describe "Affine" $ do
    describe "isomorphism" $ do
      it "1" $ property $ cmp `inverts` xy 
      it "2" $ property $ xy `inverts` cmp 
  
    describe "dot" $ do
      it "pair" $ dot @XY @XY (1, 2) (3, 6) == 15
      it "complex" $ dot @Cmp @Cmp (1 :+ 2) (3 :+ 6) == 15
      it "orthogonality" $ property $ \x ->
        not (isZero x) ==> rotate 90 x `isOrthogonal` (x :: XY)
  
    describe "cross" $ do
      it "pair" $ cross @XY @XY (1, 2) (3, 6) == 0
      it "complex" $ cross @Cmp @Cmp (1 :+ 2) (3 :+ 6) == 0
    
    describe "norm" $ do
      it "pair" $ norm @XY (3, 4) == 5
      it "complex" $ norm @Cmp (3 :+ 4) == 5

    describe "normalize" $ do
      it "pair" $ norm (normalize @XY (3, 4)) == 1
      it "complex" $ norm (normalize @Cmp (3 :+ 4)) == 1
      it "trivial" $ normalize @Cmp 0 == 0

    describe "opposite" $ 
      it "1" $ property $ \x -> x `isOpposite` opposite @XY x
        
  describe "Transformations" $ do
    describe "translate" $ do
      it "1" $ translate' @XY @XY (10, 20) (3, 4)  == (13, 24)
      it "2" $ translate' @XY @Cmp 10 (3, 4)  == (13, 4)
      it "3" $ translate' @Cmp @XY (3, 4) 10  == 13 :+ 4

    it "scale" $ scale @XY 10 (3, 4)  == (30, 40)
    it "scaleX" $ scaleX @XY 10 (3, 4) ~= (30, 4)
    it "scaleY" $ scaleY @XY 10 (3, 4) ~= (3, 40)

    describe "scaleAt" $ do
      it "1" $ scaleAt' @XY @XY (3, 4) 10 (3, 4)  == (3, 4)
      it "2" $ scaleAt' @XY @XY (0, 0) 10 (3, 4)  == (30, 40)
      it "3" $ scaleAt' @XY @XY (1, 1) 10 (3, 4)  == (21, 31)
      it "4" $ scaleAt' @XY @XY (1, 1) 10 (0, 0)  == (-9, -9)

    describe "rotate" $ do
      it "1" $ rotate @XY 0 (3, 4)  == (3, 4)
      it "2" $ rotate @XY 90 (3, 4)  ~= (-4, 3)
      it "3" $ rotate @XY 360 (3, 4)  == (3, 4)
      it "4" $ rotate @XY (asCmp (0 :+ 1)) (3, 4) ~= (-4, 3)

    describe "rotateAt" $ do
      it "1" $ rotateAt' @XY @XY (0, 0) 0 (3, 4) == (3, 4)
      it "2" $ rotateAt' @XY @XY (3, 4) 90 (3, 4) == (3, 4)
      it "3" $ rotateAt' @XY @XY (1, 0) 90 (0, 0) ~= (1, -1)
      it "4" $ rotateAt' @XY @XY (1, 1) 180 (0, 0) ~= (2, 2)

  describe "distanceTo" $ do
    let c1 = aCircle
        c2 = aCircle # scale 2
        c3 = aCircle # scale 3
    it "1" $ property $ \x -> distanceTo (c1 @-> x) c2 ~= 1
    it "2" $ property $ \x -> distanceTo (c3 @-> x) c2 ~= 1
    it "3" $ distanceTo aPoint c2 == 2
    it "4" $ distanceTo aPoint c3 == 3
    it "5" $ property $ \x a ->
      let l1 = aLine # rotate a
          l2 = l1 # translate' (normal l1 0)
      in distanceTo (l1 @-> x) l2 ~= 1
    it "6" $ property $ \x (Positive n) ->
      let t1 = regularPoly (n + 3)
          t2 = t1 # scale 2
      in distanceTo (t1 @-> x) t2 ~= cos (pi/ fromIntegral (n + 3))
      
  describe "Box" $ do
    let l = Segment (1:+2, 4:+6)
    it "0" $ box l == ((1,2),(4,6))
    it "1" $ (left.lower.corner $ l) == 1:+2
    it "2" $ (right.lower.corner $ l) == 4:+2
    it "3" $ (left.upper.corner $ l) == 1:+6
    it "4" $ (right.upper.corner $ l) == 4:+6
    it "5" $ figureHeight l == 4
    it "6" $ figureWidth l == 3

  describe "AlmostEqual" $ do
    it "1" $ property $ \i j -> let _ = (i :: Int, j :: Int)
                                in (i == j) == (i ~= j)
    it "2" $ property $ \i j -> let _ = (i :: Maybe Int, j :: Maybe Int)
                                in (i == j) == (i ~= j)

