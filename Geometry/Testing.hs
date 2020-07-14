{-# Language DerivingVia                #-}
{-# Language FlexibleInstances          #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses      #-}
{-# Language UndecidableInstances       #-}

module Geometry.Testing (
    DInt (..)
  , Parameter (..)
  , Position (..)
  , AnyPoint (..)
  , AnyCircle (..)
  , AnyAngle (..)
  , AnyLine (..)
  , AnyRay (..)
  , AnySegment (..)
  , Motion (..)
  , Nontrivial (..)
  , NonDegenerate (..)
) where

import Data.Complex
import Data.Foldable
import Data.Monoid
import Test.QuickCheck hiding (scale)
import Test.QuickCheck.Modifiers
import Data.Tuple.Extra (second)

import Geometry
import Geometry.Circle (mkCircle)
import Geometry.Base (roundUp)

infix 0 <==>
a <==> b = (a ==> b) .&&. (b ==> a)

------------------------------------------------------------

{- |  A type for an interger double number.

>>>  sample (arbitrary :: Gen DInt)
DInt 0.0
DInt 2.0
DInt 0.0
DInt (-1.0)
DInt 7.0
...
-}
newtype DInt = DInt Double deriving Show

instance Arbitrary DInt where
  arbitrary = DInt . fromIntegral <$> (arbitrary :: Gen Int)
  shrink (DInt l) = DInt <$> shrink l

{- |  A type for a double number parameterizing the `Manifold`.

>>>  sample (arbitrary :: Gen Parameter)
Parameter 0.6977012699848539
Parameter 0.5321234247634057
Parameter 9.832672492232597e-2
Parameter 0.9510866292762408
...
-}
newtype Parameter = Parameter Double deriving Show

instance Arbitrary Parameter where
  arbitrary = Parameter <$> choose (0, 1)
  shrink (Parameter l) = Parameter <$> shrink l

------------------------------------------------------------

{- |  A type for an affine position (`XY`, `Cmp`, `Point` etc).

>>>  sample (arbitrary :: Gen (Position XY))
Position (0.0,0.0)
Position (-1.0,2.0)
Position (-3.0,3.0)
Position (6.0,4.0)
Position (-1.0,-1.0
...
-}
newtype Position a = Position a
  deriving (Show, Affine, Trans, Metric, Eq)


instance (Eq a, Pnt a, Arbitrary a) =>
         Arbitrary (Position a) where
  arbitrary = Position . roundUp 1 <$> arbitrary 
  shrink = shrinkPos 0.5


shrinkPos :: (Eq a, Affine a, Metric a) => Double -> a -> [a]
shrinkPos d x = res
  where res = filter (/= x) $
              map (roundUp d) $
              takeWhile (\p -> distance x p >= d/2) $
              map (\s -> asCmp $ (1 - s) * cmp x) $
              iterate (/2) 1

------------------------------------------------------------

instance Arbitrary Direction where
  arbitrary = oneof [asDeg  <$> arbitrary, asCmp <$> arbitrary]
  shrink = shrinkPos 1

------------------------------------------------------------
instance Arbitrary Point where
  arbitrary = Point <$> arbitrary
  shrink = shrinkPos 1

{- |  A wrapper for a `Point`. -}
newtype AnyPoint = AnyPoint Point
  deriving (Show, Arbitrary)

------------------------------------------------------------

instance Arbitrary Circle where
  arbitrary = do (Position c) <- arbitrary
                 (Position r) <- arbitrary :: Gen (Position Cmp)
                 return (mkCircle (norm r) c # rotateAt' c (angle r))
                 
  shrink cir =
    do Position c <- shrink (Position (center cir))
       r <- shrink (radius cir)
       return $ mkCircle r c

{- |  A wrapper for nondegenative `Circle`. -}
newtype AnyCircle = AnyCircle Circle
  deriving Show
  deriving Arbitrary via Nontrivial Circle

------------------------------------------------------------

instance Arbitrary Angle where
  arbitrary = do (Position p) <- arbitrary
                 Angle p <$> arbitrary <*> arbitrary
                 
  shrink an =
    do Position p <- shrink (Position (refPoint an))
       s <- shrink (angleStart an)
       e <- shrink (angleEnd an)
       return $ Angle p s e

{- |  A wrapper for nontrivial `Angle`. -}
newtype AnyAngle = AnyAngle Angle
  deriving Show
  deriving Arbitrary via Nontrivial Angle

------------------------------------------------------------

instance Arbitrary Segment where
  arbitrary =
    do Position p1 <- arbitrary
       Position p2 <- arbitrary
       return $ Segment (p1, p2)
                 
  shrink l = let (p1, p2) = refPoints l
    in do Position p1' <- shrink (Position p1)
          Position p2' <- shrink (Position p2)
          return $ Segment (p1', p2')

instance Arbitrary Line where
  arbitrary = asLine <$> (arbitrary :: Gen Segment)      
  shrink l = asLine <$> shrink (asSegment l)

instance Arbitrary Ray where
  arbitrary = asRay <$> (arbitrary :: Gen Segment)      
  shrink l = asRay <$> shrink (asSegment l)

{- |  A wrapper for nontrivial `Line`. -}
newtype AnyLine = AnyLine Line
  deriving Show
  deriving Arbitrary via Nontrivial Line

{- |  A wrapper for nontrivial `Ray`. -}
newtype AnyRay = AnyRay Ray
  deriving Show
  deriving Arbitrary via Nontrivial Ray

{- |  A wrapper for nontrivial `Segment`. -}
newtype AnySegment = AnySegment Segment
  deriving Show
  deriving Arbitrary via Nontrivial Segment

------------------------------------------------------------

{- |  A wrapper for a motion `translate`, `rotate` or `reflect`. -}  
newtype Motion a = Motion (String, Endo a)

appMotion :: Motion a -> a -> a
appMotion (Motion (_, m)) = appEndo m

instance Show (Motion a) where
  show (Motion (s, _)) = s

instance Trans a => Arbitrary (Motion a) where
  arbitrary = Motion . fold <$> listOf (oneof motions)
    where
      motions = [ label "Tr " translate <$> (arbitrary :: Gen XY)
                , label "Rot " rotate <$> arbitrary
                , label "Ref " reflect <$> arbitrary ]
      label l t x = (l <> show x <> " ", Endo $ t x) 

------------------------------------------------------------

{- |  A wrapper for a nontrivial `Figure`. -}  
newtype Nontrivial a = Nontrivial a deriving
  ( Eq
  , Show
  , Figure
  , Affine
  , Trans
  , Curve
  , ClosedCurve
  , Manifold
  , PiecewiseLinear
  , Polygonal
  , APoint
  , Linear
  , Circular
  , Angular
  )


instance (Arbitrary a, Figure a) => Arbitrary (Nontrivial a) where
  arbitrary = Nontrivial <$> arbitrary `suchThat` isNontrivial
  shrink (Nontrivial l) = Nontrivial <$> filter isNontrivial (shrink l)

--------------------------------------------------------------------------------

{- |  A wrapper for a nondegenerate `Figure`. -}  
newtype NonDegenerate a = NonDegenerate a deriving
  ( Eq
  , Show
  , Figure
  , Affine
  , Trans
  , Curve
  , ClosedCurve
  , Manifold
  , PiecewiseLinear
  , Polygonal
  , APoint
  , Linear
  , Circular
  , Angular
  )

instance (Arbitrary a, PiecewiseLinear a) => Arbitrary (NonDegenerate a) where
  arbitrary = NonDegenerate <$> arbitrary `suchThat` isNondegenerate
  shrink (NonDegenerate l) = NonDegenerate <$> filter isNondegenerate (shrink l)

--------------------------------------------------------------------------------

instance Arbitrary Triangle where
  arbitrary = (`suchThat` isNontrivial) $ do
    Position p1 <- arbitrary
    Position p2 <- arbitrary
    Position p3 <- arbitrary
    pure $ Triangle [p1, p2, p3]

  shrink (Triangle [p1, p2, p3]) = filter isNontrivial $ do
    p1' <- shrink p1
    p2' <- shrink p2
    p3' <- shrink p3
    pure $ Triangle [p1', p2', p3']

instance Arbitrary RightTriangle where
  arbitrary = (`suchThat` isNondegenerate) $ do
    Positive a <- arbitrary
    Positive b <- arbitrary
    m <- arbitrary
    pure $ aRightTriangle # scaleX a # scaleY b # appMotion m
  shrink (RightTriangle t) = RightTriangle <$> shrink t
