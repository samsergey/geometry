{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex

import Geometry
import Testing

main :: IO ()
main = hspec $
  describe "Line" $ do
    describe "parametrization" $ do
      it "1" $
        property $ \(Nontrivial l) x ->
                     let _ = l ::Line in (l @-> x) ->@ l ~== x

      it "2" $ property $
        \l x -> let _ = l :: Line
                    p = l @-> x
                in l @-> (p ->@ l) ~== p

      it "3" $ line' @CN @CN 0 1 @-> 0 == 0
      it "4" $ line' @CN @CN 0 2 @-> 1 == 2
      it "5" $ line' @CN @CN 0 2 @-> 0.5 == 1

    describe "equation and similarity" $ do
      it "1" $ mkLine (0,1) == mkLine (0,1)
      it "2" $ mkLine (0,1) /= mkRay (0,1)
      it "3" $ mkLine (0,1) /= mkSegment (0,1)
      it "4" $
        property $ \(AnySegment s) m -> appMotion m s `isSimilar` s 

    describe "containing" $ do
      it "1" $
        property $ \(AnyLine l) x -> l `isContaining` (l @-> x)
        
      it "2" $
        property $ \(Nontrivial (AnyRay r)) (NonNegative x) ->
                     r `isContaining` (r @-> x)
        
      it "3" $
        property $ \(Nontrivial (AnySegment s)) x ->
                     (0 <= x && x <= 1) == (s `isContaining` (s @-> x))

    describe "tangent and family" $ do
      it "1" $ angle (line' @CN @CN 0 (1:+1)) == 45
      it "2" $ normal (line' @CN @CN 0 (1:+2)) 0 == asCmp ((-2):+1)

    describe "extendAs" $ do
      let s = segment (2,3) (4,6)
      let r = ray (2,3) (4,6)
      let l = line (2,3) (4,6)
      it "1" $ s `extendAs` Bound == s
      it "2" $ s `extendAs` Semibound == r
      it "3" $ s `extendAs` Unbound == l
      it "4" $ r `extendAs` Bound == r
      it "5" $ r `extendAs` Semibound == r
      it "6" $ r `extendAs` Unbound == l
      it "7" $ l `extendAs` Bound == l
      it "8" $ l `extendAs` Semibound == l
      it "9" $ l `extendAs` Unbound == l

    describe "at" $ do
      it "1" $ aLine # at ((1,2) :: XY) == mkLine (1:+2, 2:+2)

    describe "along" $ do
      it "1" $ aLine # at (2,3) # along 0 == line (2,3) (3,3)
      it "2" $ aLine # at (2,3) # along 90 == line (2,3) (2,4)
      it "3" $
        property $ \a p l ->
                     let _ = (a :: Angular, p :: Point, l :: Line)
                     in l # at' p # along' a == l # along' a # at' p

    describe "distanceTo" $ do
      it "1" $ origin `distanceTo` aLine == 0
      it "2" $ origin `distanceTo` (aLine # at (0,1)) == 1
      it "3" $ origin `distanceTo` (aLine # at (0,1) # along 45) ~== 1/sqrt 2
      it "4" $ origin `distanceTo` aRay == 0
      it "5" $ origin `distanceTo` (aRay # at (3,4)) == 5
      it "6" $ origin `distanceTo` (aRay # at (-1,0)) == 0
      it "7" $ origin `distanceTo` aSegment == 0
      it "8" $ origin `distanceTo` (aSegment # at (3,4)) == 5
      it "9" $ origin `distanceTo` (aSegment # at (-2,0)) == 1
      it "10" $ origin `distanceTo` (segment (-1,0) (0,1)) == 1/sqrt 2
      
    -- describe "intersections" $ do
    --   it "1" $
    --     property $ \(AnyLine l1) (AnyLine l2) ->
    --                  length (intersections l1 l2) == if l1 `isCollinear` l2 then 0 else 1
