{-# language FlexibleInstances, DeriveFunctor, DeriveFoldable, TupleSections #-}
{-# language LambdaCase,FlexibleContexts #-}

import Prelude hiding ((**))
import Data.Semigroup
import Data.Monoid
import Data.List 
import Data.Foldable
import Data.Char
import Control.Applicative


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

data F a = F a | G (F a) (F a) deriving Show

foldR :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldR f b as = fold as `appEndo` b
  where fold = foldMap (Endo . f)

foldL :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldL f b as = fold as `appEndo` b
  where fold = getDual . foldMap (Dual . Endo . flip f)

data BT a = BLeaf a | BNode (BT a) (BT a) deriving Show

instance Foldable BT where
  foldMap m t = case t of
    BLeaf x -> m x
    BNode l r -> foldMap m l <> foldMap m r

data RT b a = Leaf a | Node b [RT b a] 
  deriving (Show, Functor, Foldable)

-- instance Foldable (RT b) where
--   foldMap m t = case t of
--     Leaf x -> m x
--     Node b xs -> foldMap (foldMap m) xs

infix 3 <&>

class Functor f => Monoidal f where
  pure' :: a -> f a
  pure' x = x <$ unit

  (<&>) :: f (a -> b) -> f a -> f b
  fab <&> fa = uncurry ($) <$> (fab ** fa)

  unit :: f ()
  unit = pure' ()

  (**) :: f a -> f b -> f (a, b)
  fa ** fb = (,) <$> fa <&> fb

instance Monoidal Maybe where
  unit = Just ()
  Just x ** Just y = Just (x, y)
  _ ** _ = Nothing

instance Monoidal [] where
  unit = [()]
  xs ** ys = foldMap (\x -> (x,) <$> ys) xs

instance Monoidal ((->) r)  where
  unit = const ()
  f ** g = \r -> (f r, g r)

data Unit a = Unit deriving Show

instance Functor Unit where
  fmap _ Unit = Unit

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f = Compose . (fmap . fmap) f . getCompose

newtype Compose3 h f g a = Compose3 { getCompose3 :: h (f (g a)) }
  deriving (Show)

instance (Functor h, Functor f, Functor g) => Functor (Compose3 h f g) where
  fmap f = Compose3 . (fmap . fmap . fmap) f . getCompose3

(<$$>) :: (Functor f0, Functor f1, Functor f2) 
       => (a -> b) -> f0 (f1 (f2 a)) -> f0 (f1 (f2 b))
(<$$>) = fmap . fmap . fmap 


--unfoldr :: [a] <- b <- (Maybe (a, b) <- b)
--foldr :: ((a, b) -> b) -> b -> [a] -> b

-- cata 
fromBase b = foldr (\d r -> r * b + d ) 0 . reverse

-- ana
toBase b = reverse . unfoldr go
  where go n = case n `divMod` b of
                (0, 0) -> Nothing
                (q, r) -> Just (r, q)

--------------------------------------------------------------------------------

data Result s a = Ok a s | Fail String s
  deriving (Functor)

instance (Show a, Show s) => Show (Result s a) where
  show (Ok a s) = "Ok " <> show a <> " " <> show s
  show (Fail m s) = "Parsing error: Expected " <> m <> " in " <> show s

newtype Parser s a = Parser { run :: s -> Result s a }
  deriving Functor 


instance Semigroup a => Semigroup (Parser s a) where
  p1 <> p2 = (<>) <$> p1 <*> p2

instance Monoid a => Monoid (Parser s a) where
  mempty = pure mempty

instance Applicative (Parser s) where
  pure = Parser . Ok
  
  pf <*> px = Parser $ \s -> case run pf s of
                               Ok f s' -> run (f <$> px) s'
                               Fail m x -> Fail m x

instance Alternative (Parser s) where
  empty = Parser $ Fail ""
  
  p1 <|> p2 = Parser $ \s -> case run p1 s of
                               Fail _ _ -> run p2 s
                               x -> x

------------------------------------------------------------

next = Parser $ \case [] -> Fail "Unexpected end of line" []
                      (h : t) -> Ok h t

test p = Parser $ \case (h:t) | p h -> Ok () (h:t)
                        x -> Fail "" x

infix 3 @?
p @? n = Parser $ \s -> case run p s of
                          Fail m x -> Fail n x
                          x -> x

------------------------------------------------------------

char c = test (==c) *> next  @? "char " <> show c

digit = test isDigit *> next  @? "digit"

int :: Parser String Int
int = read <$> some digit @? "integer number"

string s = traverse char s @? s

oneof p xs = (asum $ p <$> xs) @? "one of " <> show xs

collect m p = fold <$> many ((m <$> p) <|> mempty <$ next)

only = (:[])

forAll m p = fold <$> many (only (m <$> p) <|> only next)

sepBy p s = (:) <$> p <*> many (s *> p)

spaces = many (char ' ')

sp p = spaces *> p <* spaces

data P = P Int String Bool deriving Show

str = char '"' *> many (test (/= '"') *> next) <* char '"'

bool = read <$> (string `oneof` ["True", "False"])

pP = P <$> sp int
       <*> sp str
       <*> sp bool
                                  
csv p = (sp p `sepBy` char ',') `sepBy` char '\n'

add,sub,mul :: Parser String (Int -> Int -> Int)
add = (+) <$ char '+'
sub = (-) <$ char '-'
mul = (*) <$ char '*'

------------------------------------------------------------
