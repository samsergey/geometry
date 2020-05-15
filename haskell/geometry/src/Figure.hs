{-#language GeneralizedNewtypeDeriving #-}
module Figure where

import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Complex

import Base
import Affine

------------------------------------------------------------

newtype Corner = Corner (Endo (Int, Int))
  deriving (Semigroup, Monoid)

lower =  Corner . Endo $ \(_, x) -> (-1, x)
upper =  Corner . Endo $ \(_, x) -> (1, x)
middleX =   Corner . Endo $ \(_, x) -> (0, x)
left =  Corner . Endo $ \(x, _) -> (x, -1)
right =  Corner . Endo $ \(x, _) -> (x, 1)
middleY =   Corner . Endo $ \(x, _) -> (x, 0)
middle = middleX <> middleY
corner (Corner c) = appEndo c (-1, -1)
cornerX = fst . corner
cornerY = snd . corner

------------------------------------------------------------

class Figure a where
  isTrivial :: a -> Bool
  isSimilar :: a -> a -> Bool

  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  refPoint :: a -> CN

  labelPosition :: a -> CN
  labelPosition = refPoint
  
  labelOffset :: a -> XY
  labelOffset _ = (0.5, 0.5)
  
  labelCorner :: a -> (Int, Int)
  labelCorner f = let (x, y) = labelOffset f
                  in (signum (round x), signum (round y))

------------------------------------------------------------

newtype Labeled a = Labeled ((String, First (Int, Int), First XY), a)
  deriving Functor


instance Show a => Show (Labeled a) where
  show (Labeled ((s,_,_), x)) = s <> ":" <> show x


instance Applicative Labeled where
  pure x = Labeled (mempty, x)
  Labeled (l1, f) <*> Labeled (l2, x) = Labeled (l1 <> l2, f x)


fromLabeled    (Labeled (_, x)) = x
getLabel       (Labeled ((l, _, _), _)) = l
getLabelCorner (Labeled ((_, First c, _), _)) = c
getLabelOffset (Labeled ((_, _, First o), _)) = o
appLabel l = Labeled (l, id)

label l x = appLabel (l, mempty, mempty) <*> pure x
lcorn p x  = appLabel (mempty, pure p, mempty) <*> x
lpos d x  = appLabel (mempty, mempty, pure d) <*> x

withLabeled f l = fromLabeled $ f <$> l
withLabeled2 f (Labeled (l, a)) = f a

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

instance Figure a => Figure (Labeled a) where
   refPoint = withLabeled refPoint
   isTrivial = withLabeled isTrivial
   isSimilar a b = fromLabeled $ isSimilar <$> a <*> b
   labelPosition = withLabeled labelPosition
   labelOffset lf = labelOffset (fromLabeled lf) `fromMaybe` getLabelOffset lf
   labelCorner lf = labelCorner (fromLabeled lf) `fromMaybe` getLabelCorner lf

------------------------------------------------------------
