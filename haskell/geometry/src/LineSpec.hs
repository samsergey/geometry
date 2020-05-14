{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex

import Base
import Affine
import Point
import Circle
import Line
import Geometry
import Testing

main :: IO ()
main = hspec $
  describe "Line" $ do
    describe "parametrization" $ do
      it "1" $
        property $ \(Nontrivial l) x ->
                     let types = l ::Line in (l <@ x) @> l ~== x

      it "2" $ property $
        \l x -> let types = l :: Line
                    p = l <@ x
                in l <@ (p @> l) ~== p

      it "3" $ line @CN @CN 0 1 <@ 0 == 0
      it "4" $ line @CN @CN 0 2 <@ 1 == 2
      it "5" $ line @CN @CN 0 2 <@ 0.5 == 1

    describe "equation and similarity" $ do
      it "1" $
        property $ \l -> let types = l :: Line in l == l

      it "2" $
        property $ \l x1 x2 ->
                     let p1 = l <@ x1
                         p2 = l <@ x2
                     in lineConstructor l (p1, p2) == l

      it "3" $
        property $ \(AnySegment s) m -> appMotion m s `isSimilar` s 

    describe "containing" $ do
      it "1" $
        property $ \(AnyLine l) x ->
                     isNontrivial l ==> l `isContaining` (l <@ x)
        
      it "2" $
        property $ \(AnyRay r) x ->
                     isNontrivial r ==> (0 <= x) == (r `isContaining` (r <@ x))
      it "3" $
        property $ \(AnySegment s) x ->
                     isNontrivial s ==> (0 <= x && x <= 1) == (s `isContaining` (s <@ x))
