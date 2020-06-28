{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DerivingVia #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language UndecidableInstances #-}

module Geometry.Testing where

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

newtype DInt = DInt Double deriving Show

instance Arbitrary DInt where
  arbitrary = DInt . fromIntegral <$> (arbitrary :: Gen Int)
  shrink (DInt l) = DInt <$> shrink l

newtype Parameter = Parameter Double deriving Show

instance Arbitrary Parameter where
  arbitrary = Parameter <$> oneof [pure 0, choose (0, 1), pure 1]
  shrink (Parameter l) = Parameter <$> shrink l

------------------------------------------------------------

newtype Position a = Position {getPosition :: a}
  deriving (Show, Affine, Trans, Metric, Eq)


instance (Eq a, Pnt a, Arbitrary a) =>
         Arbitrary (Position a) where
  arbitrary = Position . roundUp 1 <$> arbitrary 
  shrink = shrinkPos 0.5


shrinkPos :: (Eq a, Affine a, Metric a) => Double -> a -> [a]
shrinkPos d x = res
  where res = filter (\y -> not (y == x)) $
              map (roundUp d) $
              takeWhile (\p -> distance x p >= d/2) $
              map (\s -> asCmp $ (1 - s) * cmp x) $
              iterate (/2) 1

------------------------------------------------------------
instance Arbitrary Direction where
  arbitrary = oneof [asDeg <$> arbitrary, asCmp <$> arbitrary]
  shrink = shrinkPos 1

------------------------------------------------------------
instance Arbitrary Point where
  arbitrary = Point <$> arbitrary
  shrink = shrinkPos 1

newtype AnyPoint = AnyPoint Point
  deriving (Show, Arbitrary)

------------------------------------------------------------

instance Arbitrary Circle where
  arbitrary = do (Position c) <- arbitrary
                 (Position r) <- arbitrary :: Gen (Position Cmp)
                 return $ (mkCircle (norm r) c # rotateAt' c (angle r))
                 
  shrink cir =
    do Position c <- shrink (Position (center cir))
       r <- shrink (radius cir)
       return $ mkCircle r c

newtype AnyCircle = AnyCircle Circle
  deriving Show
  deriving Arbitrary via Nontrivial Circle

------------------------------------------------------------

instance Arbitrary Angle where
  arbitrary = do (Position p) <- arbitrary
                 s <- arbitrary
                 e <- arbitrary
                 return $ Angle p s e
                 
  shrink an =
    do Position p <- shrink (Position (refPoint an))
       s <- shrink (angleStart an)
       e <- shrink (angleEnd an)
       return $ Angle p s e

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

newtype AnyLine = AnyLine Line
  deriving Show
  deriving Arbitrary via Nontrivial Line

newtype AnyRay = AnyRay Ray
  deriving Show
  deriving Arbitrary via Nontrivial Ray

newtype AnySegment = AnySegment Segment
  deriving Show
  deriving Arbitrary via Nontrivial Segment

------------------------------------------------------------
  
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

newtype Nontrivial a = Nontrivial a deriving
  ( Eq
  , Show
  , Figure
  , Affine
  , Trans
  )

deriving instance Curve f => Curve (Nontrivial f) 

deriving instance ClosedCurve f => ClosedCurve (Nontrivial f) 

deriving instance Manifold m => Manifold (Nontrivial m)

instance (Arbitrary a, Figure a) => Arbitrary (Nontrivial a) where
  arbitrary = Nontrivial <$> arbitrary `suchThat` isNontrivial
  shrink (Nontrivial l) = Nontrivial <$> filter isNontrivial (shrink l)

--------------------------------------------------------------------------------

newtype NonDegenerate a = NonDegenerate a deriving
  ( Eq
  , Show
  , Figure
  , Affine
  , Trans
  , Polygonal
  )

deriving instance Curve f => Curve (NonDegenerate f) 
deriving instance ClosedCurve f => ClosedCurve (NonDegenerate f) 
deriving instance Manifold m => Manifold (NonDegenerate m)
deriving instance PiecewiseLinear m => PiecewiseLinear (NonDegenerate m)

instance (Arbitrary a, Figure a, PiecewiseLinear a) => Arbitrary (NonDegenerate a) where
  arbitrary = NonDegenerate <$> arbitrary `suchThat` isNondegenerate
  shrink (NonDegenerate l) = NonDegenerate <$> filter isNondegenerate (shrink l)

--------------------------------------------------------------------------------

instance Arbitrary Triangle where
  arbitrary = t `suchThat` isNontrivial
    where t = do
            Position p1 <- arbitrary
            Position p2 <- arbitrary
            Position p3 <- arbitrary
            pure $ Triangle [p1,p2,p3]
  shrink (Triangle [p1,p2,p3]) = filter isNontrivial ts
    where ts = do
            p1' <- shrink p1
            p2' <- shrink p2
            p3' <- shrink p3
            pure $ Triangle [p1', p2', p3']

instance Arbitrary RightTriangle where
  arbitrary = (`suchThat` isNondegenerate) $ do
    a <- asDeg <$> choose (0, 90)
    m <- arbitrary
    Positive s <- arbitrary
    pure $ rightTriangle a # scale s # appMotion m
  shrink (RightTriangle t) = RightTriangle <$> shrink t
