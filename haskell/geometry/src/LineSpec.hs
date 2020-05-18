{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex

import Geometry
import Testing
g
main :: IO ()
main = hspec $
  describe "Line" $ do
    describe "parametrization" $ do
      it "1" $
        property $ \(Nontrivial l) x ->
                     let _ = l ::Line in (l .@ x) @. l ~== x

      it "2" $ property $
        \l x -> let _ = l :: Line
                    p = l .@ x
                in l .@ (p @. l) ~== p

      it "3" $ line @CN @CN 0 1 .@ 0 == 0
      it "4" $ line @CN @CN 0 2 .@ 1 == 2
      it "5" $ line @CN @CN 0 2 .@ 0.5 == 1

    describe "equation and similarity" $ do
      it "1" $
        property $ \l -> l == (l :: Line)

      it "2" $
        property $ \(AnyLine l) x1 x2 -> x1 /= x2 ==>
                     let p1 = l .@ x1
                         p2 = l .@ x2
                     in Line (p1, p2) == l

      it "3" $
        property $ \(AnyRay r) (Positive x) ->
                     let p1 = r .@ 0
                         p2 = r .@ x
                     in Ray (p1, p2) == r

      it "4" $
        property $ \(AnySegment s) m -> appMotion m s `isSimilar` s 

    describe "containing" $ do
      it "1" $
        property $ \(AnyLine l) x -> l `isContaining` (l .@ x)
        
      it "2" $
        property $ \(Nontrivial (AnyRay r)) x ->
                     (0 <= x) == (r `isContaining` (r .@ x))
      it "3" $
        property $ \(Nontrivial (AnySegment s)) x ->
                     (0 <= x && x <= 1) == (s `isContaining` (s .@ x))

    describe "tangent and family" $ do
      it "1" $ angle (line @CN @CN 0 (1:+1)) == 45
      it "2" $ normal (line @CN @CN 0 (1:+2)) 0 == asCmp ((-2):+1)

    describe "extendAs" $ do
      let s = segment @XY @XY (2,3) (4,6)
      let r = ray @XY @XY (2,3) (4,6)
      let l = line @XY @XY (2,3) (4,6)
      it "1" $ s `extendAs` Segment == s
      it "2" $ s `extendAs` Ray == r
      it "3" $ s `extendAs` Line == l
      it "4" $ r `extendAs` Segment == r
      it "5" $ r `extendAs` Ray == r
      it "6" $ r `extendAs` Line == l
      it "7" $ l `extendAs` Segment == l
      it "8" $ l `extendAs` Ray == l
      it "9" $ l `extendAs` Line == l

    describe "at" $ do
      it "1" $ aLine <| at ((1,2) :: XY) == Line (1:+2, 2:+2)

    describe "along" $ do
      it "1" $ aLine <| at ((2,3) :: XY) <| along (asDeg 0) == line @XY @XY (2,3) (3,3)
      it "2" $ aLine <| at ((2,3) :: XY) <| along (asDeg 90) == line @XY @XY (2,3) (2,4)
      it "3" $
        property $ \a p l ->
                     let _ = (a :: Angular, p :: Point, l :: Line)
                     in l <| at p <| along a == l <| along a <| at p

    -- describe "intersections" $ do
    --   it "1" $
    --     property $ \(AnyLine l1) (AnyLine l2) ->
    --                  length (intersections l1 l2) == if l1 `isCollinear` l2 then 0 else 1
