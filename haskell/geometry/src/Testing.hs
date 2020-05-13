{-# Language TypeApplications #-}
module Testing where

import Data.Complex
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Base
import Affine
import Point
import Circle
import Line
import Geometry

newtype Position a = Position {getPosition :: a}
  deriving Show

instance Affine a => Affine (Position a) where
  cmp = cmp . getPosition
  fromCN = Position . fromCN

instance Trans a => Trans (Position a) where
  transform t (Position p) = Position (transform t p)


instance (Trans a, Affine a, Arbitrary a) => Arbitrary (Position a) where
  arbitrary = Position <$> roundUp 1 <$> arbitrary 
  shrink = shrinkPos 1


shrinkPos :: Affine a => Double -> a -> [a]
shrinkPos d x = res
  where res = map (roundUp d) $
              takeWhile (\p -> distance x p >= d/2) $
              map (\s -> fromCN $ (1 - s) * cmp x) $
              iterate (/2) 1


instance Arbitrary Angular where
  arbitrary = oneof [Deg <$> arbitrary, Cmp <$> arbitrary]
  shrink = shrinkPos 1


instance Arbitrary Point where
  arbitrary = Point <$> arbitrary
  shrink = shrinkPos 1

instance Arbitrary Circle where
  arbitrary = do r <- arbitrary
                 Position c <- arbitrary
                 return $ mkCircle (abs r) c
                 
  shrink (Circle r c _ _) = do r' <- shrink r
                               Position c' <- shrink (Position c)
                               return $ mkCircle r' c'

instance Arbitrary Line where
  arbitrary = do Position p1 <- arbitrary
                 Position p2 <- arbitrary
                 return $ line @XY @XY p1 p2
                 
  shrink (Line p1 p2) = do Position p1' <- shrink (Position p1)
                           Position p2' <- shrink (Position p2)
                           return $ line p1' p2'
