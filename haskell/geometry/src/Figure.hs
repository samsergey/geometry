{-#language GeneralizedNewtypeDeriving #-}
module Figure where

import Control.Applicative

import Base
import Affine

class Figure a where
  isTrivial :: a -> Bool
  isSimilar :: a -> a -> Bool

  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  refPoint :: a -> CN

  labelPosition :: a -> CN
  labelPosition = refPoint
  
  labelOffset :: a -> Double
  labelOffset _ = 0.5
  
  labelOrientation :: a -> Angular
  labelOrientation _ = Deg 125

------------------------------------------------------------

newtype Labeled a = Labeled (String, a)
  deriving (Show, Functor)

instance Applicative Labeled where
  pure x = Labeled (mempty, x)
  Labeled (l1, f) <*> Labeled (l2, x) = Labeled (l1 <> l2, f x)
  

getLabel (Labeled (l, _)) = l
fromLabeled  (Labeled (_, x)) = x
x `label` l = Labeled (l, x) 

withLabeled f l = fromLabeled $ f <$> l
withLabeled2 f (Labeled (l, a)) b = f a b

instance Eq a => Eq (Labeled a) where
  a == b = fromLabeled $ (==) <$> a <*> b

instance Trans a => Trans (Labeled a) where
  transform t x = transform t <$> x

instance Affine a => Affine (Labeled a) where
  cmp  = withLabeled cmp
  fromCN c = pure (fromCN c)

instance Curve a => Curve (Labeled a) where
  param = withLabeled2 param
  locus = withLabeled2 locus
  normal = withLabeled2 normal

------------------------------------------------------------
