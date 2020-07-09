{-# Language TypeApplications #-}

module LineSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Invariant
import Data.Complex
import Data.Fixed (mod')
import Data.Maybe
import Geometry
import Geometry.Testing

spec :: Spec
spec = describe "Line" $ do
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
      it "1" $ property $ \(AnyLine l) x -> (l @-> x) ->@ l ~= x

      it "2" $ property $ \(AnyLine l) x ->
        let p = l @-> x
        in l @-> (p ->@ l) ~= p

      it "3" $ line' @Cmp @Cmp 0 1 @-> 0 == 0
      it "4" $ line' @Cmp @Cmp 0 2 @-> 1 == 2
      it "5" $ line' @Cmp @Cmp 0 2 @-> 0.5 == 1

      it "6" $ property $ \(AnyLine l) x ->
        isJust (l @->? x)

      it "7" $ property $ \(AnyRay r) x ->
        isJust (r @->? x) <==> x >= 0

      it "8" $ property $ \(AnySegment s) x ->
        let x' = 2 * cos x
        in isJust (s @->? x') <==> 0 <= x' && x' <= 1

      it "9" $ property $ \(AnyLine l) (AnyPoint p) -> isJust (p ->@? l)

      it "10" $ property $ \(AnyRay r) (AnyPoint p) ->
        let an = deg . angleValue $ angleBetween r (r # through' p)
        in isJust (p ->@? r)
           <==>
           (0 <= an && an <= 90) || (270 <= an && an <= 360)

      it "11" $ property $ \(AnySegment s) (AnyPoint p) ->
        let (p1, p2) = refPoints s
            an1 = deg . angleValue . innerAngle $ angleWithin p p1 p2
            an2 = deg . angleValue . innerAngle $ angleWithin p p2 p1
        in isJust (p ->@? s)
           <==>
           (0 <= an1 && an1 <= 90) && (0 <= an2 && an2 <= 90)
           

    describe "containing" $ do
      it "1.1" $
        property $ \(AnyLine l) x -> l `isContaining` (l @-> x)
      it "1.2" $
        property $ \(AnyLine l) -> l `isContaining` refPoint l

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
      it "1" $ angle (line' @Cmp @Cmp 0 (1:+1)) == 45
      it "2" $ normal (line' @Cmp @Cmp 0 (1:+2)) 0 == asCmp ((-2):+1)

    describe "unit" $ do
      it "1" $ unit (line' @Cmp @Cmp 0 (3:+4)) == 5
      it "2" $ unit (asCmp 1 :: Ray) == 1
      it "3" $ unit (asCmp 1 :: Segment) == 1
      it "4" $ property $ \l -> unit l == norm (l :: Line)
      it "5" $ property $ \l -> unit l == norm (l :: Ray)
      it "6" $ property $ \l -> unit l == norm (l :: Segment)

    describe "at" $
      it "1" $ aLine # at ((1,2) :: XY) == line (1,2) (2,2)

    describe "along" $ do
      it "1" $ aLine # at (2,3) # along 0 == line (2,3) (3,3)
      it "2" $ aLine # at (2,3) # along 90 == line (2,3) (2,4)
      it "3" $
        property $ \a p l ->
                     let _ = (a :: Direction, p :: Point, l :: Line)
                     in l # at' p # along' a == l # along' a # at' p

    describe "extendToLength" $ 
      it "1" $ property $ \(AnySegment s) d ->
        unit (s # extendToLength d) ~= abs d

    describe "extendTo" $ do
      it "1" $ property $ \(AnySegment s) (AnyLine l) ->
        (asRay s `isIntersecting` l) ==>
        l `isContaining` ((s # extendTo l) @-> 1)
      it "2" $ property $ \(AnySegment s) (AnyCircle c) ->
        (asRay s `isIntersecting` c) ==>
        c `isContaining` ((s # extendTo c) @-> 1)

    describe "heightTo" $
      it "1" $ property $ \(AnyPoint p) (AnyLine l) ->
        not (l `isContaining` cmp p) ==>
        fromMaybe False $ do
          h <- l # heightFrom p
          return $ l `isContaining` end h && l `isOrthogonal` h

      -- it "2" $ property $ \(AnyPoint p) (AnyCircle c) ->
      --   not (c `isContaining` p) ==>
      --   fromMaybe False $ do
      --     h <- p # heightTo c
      --     let n = normal c (project c (end h))
      --     return $ c `isContaining` (end h) && h `isCollinear` n


    describe "distanceTo" $ do
      it "1" $ origin `distanceTo` aLine == 0
      it "2" $ origin `distanceTo` (aLine # at (0,1)) == 1
      it "3" $ origin `distanceTo` (aLine # at (0,1) # along 45) ~= 1/sqrt 2
      it "4" $ origin `distanceTo` aRay == 0
      it "5" $ origin `distanceTo` (aRay # at (3,4)) == 5
      it "6" $ origin `distanceTo` (aRay # at (-1,0)) == 0
      it "7" $ origin `distanceTo` aSegment == 0
      it "8" $ origin `distanceTo` (aSegment # at (3,4)) ~= 5
      it "9" $ origin `distanceTo` (aSegment # at (-2,0)) ~= 1
      it "10" $ origin `distanceTo` segment (-1,0) (0,1) ~= 1/sqrt 2
      
    describe "intersections" $ do
      it "1" $
        property $ \(AnyLine l1) (AnyLine l2) ->
             isIntersecting l1 l2
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
      it "4.7" $ s `intersections` (s # rotate 90 # translate (0.5,0.5) ) ~= [0.5:+0.5]

      it "5.1" $
        property $ \(AnyLine l) (AnyRay r) ->
            if l `isCollinear` r
            then null (l `intersections` r) ||
                 (r `intersections` l ~= [start r]
                   && l `intersections` r ~= [start l])
            else l `intersections` r ~= r `intersections` l

      it "5.2" $
        property $ \(AnyLine l) (AnySegment s) ->
             if l `isCollinear` s
             then null (l `intersections` s) ||
                  (s `intersections` l ~= [start s]
                   && l `intersections` s ~= [start l])
             else l `intersections` s ~= s `intersections` l

      it "5.3" $
          property $ \(AnySegment s) (AnyRay r) ->
             if s `isCollinear` r
             then null (s `intersections` r) ||
                  (r `intersections` s ~= [start r]
                   && s `intersections` r ~= [start s])
             else s `intersections` r ~= r `intersections` s

    describe "clipping" $ do
      it "1" $ aLine # rotate 45 # clipBy aSquare == [Segment (0, 1:+1)]
      it "2" $ aLine # rotate 45 # at (0.5,0.5) # clipBy aSquare ==
                [Segment (0, 1:+1)]
      it "3" $ aRay # rotate 45 # at (0.5,0.5) # clipBy aSquare ==
                [Segment (0.5:+0.5, 1:+1)]
      it "4" $ aSegment # rotate 45 # at (0.1,0.1) # clipBy aSquare ==
                [Segment (0.1:+0.1, 0.8071067811865475:+0.8071067811865475)]
      it "5" $ aLine # rotate 45 # at (-0.5,0) # clipBy aSquare ==
                [Segment (0:+0.5, 0.5:+1)]
