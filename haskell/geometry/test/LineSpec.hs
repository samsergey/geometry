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
        in case r @->? x of
          Just _ -> x >= 0
          Nothing -> x < 0

      it "8" $ property $ \(Nontrivial s) x ->
        let types = s :: Segment
        in case s @->? x of
          Just _ -> 0 <= x && x <= 1
          Nothing -> x < 0 || 1 < x

      it "9" $ property $ \(Nontrivial l) p ->
        let types = (p :: Point, l :: Line)
        in isJust (p ->@? l)

      it "10" $ property $ \(Nontrivial r) p ->
        let types = (p :: Point, r :: Ray)
            an = deg . angleValue $ angleBetween r (r # through' p)
        in case p ->@? r of
          Just _ -> (0 <= an && an <= 90) || (270 <= an && an <= 360)
          Nothing -> 90 < an && an < 270

      it "11" $ property $ \(Nontrivial s) p ->
        let types = (p :: Point, s :: Segment)
            (p1, p2) = refPoints s
            an1 = deg . angleValue $ angleWithin p p1 p2
            an2 = deg . angleValue $ angleWithin p p2 p1
        in case p ->@? s of
          Just _ -> (0 <= an1 && an1 <= 90) && (0 <= an2 && an2 <= 90)
          Nothing -> an1 > 90 || an2 > 90


    describe "containing" $ do
      it "1" $
        property $ \l x -> (l :: Line) `isContaining` (l @-> x)

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

    describe "at" $ do
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
      it "10" $ origin `distanceTo` (segment (-1,0) (0,1)) ~== 1/sqrt 2
      
    -- describe "intersections" $ do
    --   it "1" $
    --     property $ \(AnyLine l1) (AnyLine l2) ->
    --                  length (intersections l1 l2) == if l1 `isCollinear` l2 then 0 else 1
