{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Testing where

import Data.Complex
import Data.Foldable
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Data.Tuple.Extra (second)

import Geometry

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
  deriving (Show, Affine, Trans)


instance (Trans a, Affine a, Arbitrary a) =>
         Arbitrary (Position a) where
  arbitrary = Position . roundUp 1 <$> arbitrary 
  shrink = shrinkPos 1


shrinkPos :: Affine a => Double -> a -> [a]
shrinkPos d x = res
  where res = map (roundUp d) $
              takeWhile (\p -> distance x p >= d/2) $
              map (\s -> asCmp $ (1 - s) * cmp x) $
              iterate (/2) 1

------------------------------------------------------------
instance Arbitrary Angular where
  arbitrary = oneof [asDeg <$> arbitrary, asCmp <$> arbitrary]
  shrink = shrinkPos 1

------------------------------------------------------------
instance Arbitrary Point where
  arbitrary = Point <$> arbitrary
  shrink = shrinkPos 1

instance Arbitrary Circle where
  arbitrary = do Position c <- arbitrary
                 Position p <- arbitrary
                 w <- signum <$> arbitrary
                 return $ Circle c p w
                 
  shrink (Circle c p w) =
    do Position c' <- shrink (Position c)
       Position p' <- shrink (Position p)
       return $ Circle c' p' w

shrinkLine l = let (p1, p2) = refPoints l
  in do Position p1' <- shrink (Position p1)
        Position p2' <- shrink (Position p2)
        return $ Line (bounding l) (p1', p2')

------------------------------------------------------------

instance Arbitrary Line where
  arbitrary =
    do Position p1 <- arbitrary
       Position p2 <- arbitrary
       constr <- elements [Unbound, Semibound, Bound]
       return $ Line constr (p1, p2)
                 
  shrink l = let (p1, p2) = refPoints l
    in do Position p1' <- shrink (Position p1)
          Position p2' <- shrink (Position p2)
          return $ Line (bounding l) (p1', p2')

------------------------------------------------------------

newtype AnySegment = AnySegment Line
  deriving (Eq, Show, Figure, Trans, Affine, Curve)

newtype AnyLine = AnyLine Line 
  deriving (Show, Eq, Figure, Trans, Affine, Curve)

newtype AnyRay = AnyRay Line 
  deriving (Eq, Show, Figure, Trans, Affine, Curve)

instance Arbitrary AnyLine where
  arbitrary = AnyLine . Line Unbound <$> arbitrary
  shrink (AnyLine l) = AnyLine <$> shrink l

instance Arbitrary AnySegment where
  arbitrary = AnySegment . Line Bound <$> arbitrary
  shrink (AnySegment l) = AnySegment <$> shrink l

instance Arbitrary AnyRay where
  arbitrary = AnyRay . Line Semibound <$> arbitrary
  shrink (AnyRay l) = AnyRay <$> shrink l

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
  (Eq, Show, Figure, Affine, Trans, Curve)


instance (Arbitrary a, Figure a) => Arbitrary (Nontrivial a) where
  arbitrary = Nontrivial <$> arbitrary `suchThat` isNontrivial
  shrink (Nontrivial l) = Nontrivial <$> shrink l
