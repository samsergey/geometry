{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex
import Data.Fixed (mod')
import Data.Maybe

import Geometry
import Testing

main :: IO ()
main = hspec $
  describe "Line" $ do

    describe "Affinity" $ do
      it "1" $ (asCmp 1 :: Line) == Line (0,1)
      it "2" $ (asCmp 1 :: Ray) == Ray (0,1)
      it "3" $ (asCmp 1 :: Segment) == Segment (0,1)
      it "4" $ property $ \l -> asCmp (cmp l) # at' (refPoint l) == (l :: Line)
      it "5" $ property $ \l -> asCmp (cmp l) # at' (refPoint l) == (l :: Ray)
      it "5" $ property $ \l -> asCmp (cmp l) # at' (refPoint l) == (l :: Segment)
      it "6" $ property $ \x -> cmp (asCmp x :: Line) == x
      it "7" $ property $ \x -> cmp (asCmp x :: Ray) == x
      it "8" $ property $ \x -> cmp (asCmp x :: Segment) == x
      
    describe "parametrization" $ do
      it "1.1" $ property $ \(Nontrivial l) x ->
        let types = l ::Line
        in (l @-> x) ->@ l ~== x

      it "1.2" $ property $ \(Nontrivial r) x ->
        let types = r :: Ray
        in (r @-> x) ->@ r ~== max 0 x

      it "1.3" $ property $ \(Nontrivial s) x ->
        let types = s :: Segment
        in (s @-> x) ->@ s ~== (0 `max` x) `min` 1

      it "2.1" $ property $ \l x ->
        let types = l :: Line
            p = l @-> x
        in l @-> (p ->@ l) ~== p

      it "2.2" $ property $ \r x ->
        let types = r :: Ray
            p = r @-> x
        in r @-> (p ->@ r) ~== p

      it "2.3" $ property $ \s x ->
        let types = s :: Segment
            p = s @-> x
        in s @-> (p ->@ s) ~== p
      it "3" $ line' @CN @CN 0 1 @-> 0 == 0
      it "4" $ line' @CN @CN 0 2 @-> 1 == 2
      it "5" $ line' @CN @CN 0 2 @-> 0.5 == 1

      it "6" $ property $ \(Nontrivial l) x ->
        let types = l :: Line
        in isJust (l @->? x)

      it "7" $ property $ \(Nontrivial r) x ->
        let types = r :: Ray
        in isJust (r @->? x) <==> x >= 0

      it "8" $ property $ \(Nontrivial s) x ->
        let types = s :: Segment
            x' = 2 * cos x
        in isJust (s @->? x') <==> 0 <= x' && x' <= 1

      it "9" $ property $ \(Nontrivial l) p ->
        let types = (p :: Point, l :: Line)
        in isJust (p ->@? l)

      it "10" $ property $ \(Nontrivial r) p ->
        let types = (p :: Point, r :: Ray)
            an = deg . angleValue $ angleBetween r (r # through' p)
        in isJust (p ->@? r)
           <==>
           (0 <= an && an <= 90) || (270 <= an && an <= 360)

      it "11" $ property $ \(Nontrivial s) p ->
        let types = (p :: Point, s :: Segment)
            (p1, p2) = refPoints s
            an1 = angleValue . innerAngle $ angleWithin p p1 p2
            an2 = angleValue . innerAngle $ angleWithin p p2 p1
        in isJust (p ->@? s)
           <==>
           (0 <= an1 && an1 <= 90) && (0 <= an2 && an2 <= 90)
           

    describe "containing" $ do
      it "1.1" $
        property $ \l x -> (l :: Line) `isContaining` (l @-> x)
      it "1.2" $
        property $ \l -> (l :: Line) `isContaining` refPoint l

      it "2.1" $ aRay `isContaining` ((0,0) :: XY)
      it "2.2" $ aRay `isContaining` ((1,0) :: XY)
      it "2.5" $ not $ aRay `isContaining` ((-1,0) :: XY)
      it "2.6" $ not $ aRay `isContaining` ((1,1) :: XY)
       
      it "3.1" $ aSegment `isContaining` ((0,0) :: XY)
      it "3.2" $ aSegment `isContaining` ((1,0) :: XY)
      it "3.3" $ aSegment `isContaining` (aSegment @-> 0)
      it "3.4" $ aSegment `isContaining` (aSegment @-> 1)
      it "3.5" $ not $ aSegment `isContaining` ((-1,0) :: XY)
      it "3.6" $ not $ aSegment `isContaining` ((3,0) :: XY)

    describe "tangent and family" $ do
      it "1" $ angle (line' @CN @CN 0 (1:+1)) == 45
      it "2" $ normal (line' @CN @CN 0 (1:+2)) 0 == asCmp ((-2):+1)

    describe "at" $
      it "1" $ aLine # at ((1,2) :: XY) == line (1,2) (2,2)

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
      it "8" $ origin `distanceTo` (aSegment # at (3,4)) ~== 5
      it "9" $ origin `distanceTo` (aSegment # at (-2,0)) ~== 1
      it "10" $ origin `distanceTo` segment (-1,0) (0,1) ~== 1/sqrt 2
      
    describe "intersections" $ do
      it "1" $
        property $ \(Nontrivial l1) (Nontrivial l2) ->
          let types = (l1 :: Line, l2 :: Line)
          in isIntersecting l1 l2
             <==>
             not (l1 `isCollinear` l2) || l1 `isContaining` refPoint l2
      let r = Ray (0, 1:+1)
          s = Segment (0, 1:+1)
      it "2.1" $ r `intersections` (r # translate (3,0)) # null
      it "2.2" $ r `intersections` (r # rotate 90 # translate (2,0) ) == [1:+1]
      it "2.3" $ r `intersections` (r # rotate 90 # translate (-1,0) ) # null
      it "3.1" $ r `intersections` (s # translate (3,0)) # null
      it "3.2" $ r `intersections` (s # rotate 90 # translate (2,0) ) == [1:+1]
      it "3.3" $ r `intersections` (s # rotate 90 # translate (2.1,0) ) # null
      it "3.4" $ r `intersections` (s # rotate 90 # translate (1,1) ) == [1:+1]
      it "4.4" $ s `intersections` (s # rotate 90 # translate (1,1) ) == [1:+1]
      it "4.5" $ s `intersections` (s # rotate 90) == [0]
      it "4.6" $ s `intersections` (s # rotate 90 # translate (1.01,1.01) ) # null
      it "4.7" $ s `intersections` (s # rotate 90 # translate (0.5,0.5) ) ~== [0.5:+0.5]

      it "5" $
        property $ \(Nontrivial l) (Nontrivial r) (Nontrivial s) ->
          let types = (l :: Line, r :: Ray, s :: Segment)
          in l `intersections` r == r `intersections` l &&
             l `intersections` s == s `intersections` l &&
             s `intersections` r == r `intersections` s

    describe "clipping" $ do
      it "1" $ (aLine # rotate 45) `clipBy` aSquare == [Segment (0, 1:+1)]
      it "2" $ ((aLine # rotate 45 # at (0.5,0.5)) `clipBy` aSquare ==
                [Segment (0, 1:+1)])
      it "3" $ ((aRay # rotate 45 # at (0.5,0.5)) `clipBy` aSquare ==
                [Segment (0.5:+0.5, 1:+1)])
      it "4" $ ((aSegment # rotate 45 # at (0.1,0.1)) `clipBy` aSquare ==
                [Segment (0.1:+0.1, 0.8071067811865475:+0.8071067811865475)])
      it "5" $ ((aLine # rotate 45 # at (-0.5,0)) `clipBy` aSquare ==
                [Segment (0:+0.5, 0.5:+1)])
