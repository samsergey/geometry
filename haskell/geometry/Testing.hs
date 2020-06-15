{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DerivingVia #-}

module Testing where

import Data.Complex
import Data.Foldable
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Data.Tuple.Extra (second)

import Geometry

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
  deriving (Show, Affine, Trans, Eq)


instance (Eq a, Trans a, Affine a, Arbitrary a) =>
         Arbitrary (Position a) where
  arbitrary = Position . roundUp 1 <$> arbitrary 
  shrink = shrinkPos 0.5


shrinkPos :: (Eq a, Affine a) => Double -> a -> [a]
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
                 (Position r) <- arbitrary :: Gen (Position CN)
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
  , Manifold
  , Curve
  , ClosedCurve )


instance (Arbitrary a, Figure a) => Arbitrary (Nontrivial a) where
  arbitrary = Nontrivial <$> arbitrary `suchThat` isNontrivial
  shrink (Nontrivial l) = Nontrivial <$> filter isNontrivial (shrink l)

