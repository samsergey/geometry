{-# language FlexibleInstances, DeriveFunctor #-}

import Data.Semigroup
import Data.Monoid
import Data.List 

data Optional b a = Ok a | Fail b
  deriving Show

instance Functor (Optional a) where
  f `fmap` Ok x = Ok (f x)
  _ `fmap` Fail y = Fail y

instance Applicative (Optional a) where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Fail y <*> _ = Fail y
  _ <*> Fail y = Fail y

infixr 5 :::
data List a = Empty
            | a ::: List a


instance Show a => Show (List a) where
  show Empty = "{}"
  show (a ::: b) = show a <> "; " <> show b

instance Semigroup (List a) where
  Empty <> x = x
  x <> Empty = x
  (h ::: t) <> lst = h ::: (t <> lst)

instance Monoid (List a) where
  mempty = Empty

instance Functor List where
  fmap f Empty = Empty
  fmap f (h ::: t) = f h ::: fmap f t

instance Applicative List where
  pure x = x ::: Empty
  fs <*> xs = case fs of
                Empty -> Empty
                fh ::: ft -> (fh <$> xs) <> (ft <*> xs)

data Tag s a = Tag s a
  deriving (Show, Functor)

instance Semigroup s => Applicative (Tag (Maybe s)) where
  pure = Tag Nothing
  Tag a f <*> Tag b x = Tag (a <> b) (f x)

newtype Func r a = Func (r -> a)

instance Functor (Func r) where
  fmap g (Func f) = Func (g . f)

instance Applicative (Func r) where
  pure = Func . const
  Func rab <*> Func ra = Func rb
    where rb r = (rab r <$> ra) r

