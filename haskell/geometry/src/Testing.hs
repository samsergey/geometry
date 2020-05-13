module Testing where

import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Base
import Affine
import Geometry

newtype Position a = Position {getPosition :: a}
  deriving Show

instance Affine a => Affine (Position a) where
  cmp = cmp . getPosition
  fromCN = Position . fromCN

instance Trans a => Trans (Position a) where
  transform t (Position p) = Position (transform t p)


instance (Trans a, Affine a, Arbitrary a) => Arbitrary (Position a) where
  arbitrary = Position <$> arbitrary
  shrink = shrinkPos 1

shrinkPos :: Affine a => Double -> a -> [a]
shrinkPos d x = map (roundUp d) $
                takeWhile (\p -> distance x p >= d/2) $
                map (* cmp x) $
                map (1 -) $
                iterate (/2) 1


instance Arbitrary Point where
  arbitrary = Point <$> arbitrary
  shrink = shrinkPos 1

instance Arbitrary Circle where
  arbitrary = mkCircle <$> (abs <$> arbitrary) <*> arbitrary
  shrink (Circle r c _ _) = do r' <- shrink r
                               Position c' <- shrink (Position c)
                               return $ mkCircle r' c'
