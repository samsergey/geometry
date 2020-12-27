{-# language FlexibleInstances, DeriveFunctor, DeriveFoldable, TupleSections #-}
{-# language LambdaCase,FlexibleContexts #-}
{-# language DeriveTraversable,TupleSections #-}

import Prelude hiding ((**))
import Data.Semigroup
import Data.Monoid
import Data.List 
import Data.Foldable
import Data.Char
import Control.Applicative
import Control.Monad

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

chainr p op = p <**> fmany (op <*> p)
  where fmany x = appEndo . foldMap Endo <$> many x

chainl p op = p <**> fmany (flip <$> op <*> p)
  where fmany x = appEndo . getDual . foldMap (Dual . Endo) <$> many x

between [ch1, ch2] p = char ch1 *> p <* char ch2

sp p = spaces *> p <* spaces

data P = P Int String Bool deriving Show

str = between "\"\"" $ many (test (/= '"') *> next)

bool = read <$> (string `oneof` ["True", "False"])

pPerson = P <$> sp int
          <*> sp str
          <*> sp bool
                                  
csv p = (sp p `sepBy` char ',') `sepBy` char '\n'

--add,sub,mul :: Parser String (Int -> Int -> Int)
-- add = (+) <$ char '+'
-- sub = (-) <$ char '-'
-- mul = (*) <$ char '*'
-- frac = div <$ char '/'
-- neg = negate <$ char '-'
-- number = int



-- add = Add <$ char '+'
-- sub = Sub <$ char '-'
-- mul = Mul <$ char '*'
-- frac = Div <$ char '/'
-- neg = Neg <$ char '-'
-- number = N <$> int

data Exp a = N a
           | Add (Exp a) (Exp a)
           | Sub (Exp a) (Exp a)
           | Mul (Exp a) (Exp a)
           | Div (Exp a) (Exp a)
           | Neg (Exp a)
  deriving (Show, Functor)

class Calc a where
  add :: Parser String (a -> a -> a)
  sub :: Parser String (a -> a -> a)
  mul :: Parser String (a -> a -> a)
  frac :: Parser String (a -> a -> a)
  neg :: Parser String (a -> a)
  number :: Parser String a
  eval :: Parser String a
  eval = expr
    where
      expr = term `chainl` (add <|> sub)
      term = mult `chainl` (mul <|> frac)
      mult = number
        <|> between "()" expr
        <|> neg <*> mult

-- newtype RPN = RPN [String] deriving Show

-- instance Calc RPN where
--   add = (\x y -> x <> y <> ["+"]) <$ char '+'
--   sub = (\x y -> x <> y <> ["-"]) <$ char '-'
--   mul = (\x y -> x <> y <> ["*"]) <$ char '*'
--   frac = (\x y -> x <> y <> ["/"]) <$ char '/'
--   neg = (\x -> x <> ["n"]) <$ char '-'
--   number = only . show <$> int
  
--------------------------------------------------------------------------------
-- Hask

-- fmap ::   (a -> b) -> (f a -> f b)

-- ap   :: f (a -> b) -> (f a -> f b)  ~  ** :: f a -> f b -> f (a,b)
-- pure :: a -> f a                    ~  unit :: f ()

-- bind :: (a -> f b) -> (f a -> f b)  ~  join :: f (f a) -> f a
-- bind f fa = join (f <$> fa)         ~  join x = ...



newtype State s a = Prog (s -> (a, s))

runProg :: State s a -> s -> (a, s)
runProg (Prog f) = f

instance Functor (State s) where
  fmap g prog = Prog prog'
    where prog' s = let (a, s') = runProg prog s
                    in (g a, s')

instance Applicative (State s) where
  pure x = Prog $ \s -> (x, s)
  sf <*> sx = Prog $ \s -> let (f, s') = runProg sf s
                        in runProg (f <$> sx) s'

instance Monad (State s) where
  prog >>= f = Prog $ \s -> let (x, s') = runProg prog s
                         in runProg (f x) s'
  
put x = Prog $ \s -> ((), x)
get = Prog $ \s -> (s, s)
modify f = Prog $ \s -> ((), f s)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Traversable, Foldable, Functor)

execProg s p = fst $ runProg p s
evalProg s p = snd $ runProg p s

tree 0 = modify (+1) *> (Leaf <$> get)
tree n = Node <$> tree (n-1) <*> tree (n-1)

nextRND k = do x <- get
               put $ (x * a + c) `mod` m
               return $ x `mod` k
  where (a, c, m) = (141, 28411, 134456)

nextRNDF = (\r -> fromIntegral r / 1000) <$> nextRND 1000

randomPair = (,) <$> nextRNDF <*> nextRNDF

randTree n = execProg 42 $ prog n
  where prog 0 = Leaf <$> nextRND 100
        prog n = Node <$> prog (n-1) <*> prog (n-1)

montePi n s = execProg s prog
  where
    countMatches = length . filter (\(x, y) -> sqrt (x*x + y*y) < 1)
    prog = countMatches <$> sequenceA (replicate n randomPair)
          
--------------------------------------------------------------------------------

newtype Dist a = Dist [(a, Int)]
  deriving (Show, Functor)

getDist (Dist d) = d

prop' a = maybe 0 id . lookup a . getDist . clean
prop a d = fromIntegral (prop' a d) / fromIntegral (sum (snd <$> getDist d))

instance Eq a => Semigroup (Dist a) where
 Dist [] <> d2 = d2
 Dist d1 <> Dist d2 = Dist $ foldl addDist d1 d2
   where
     addDist [] (x,n) = [(x,n)]
     addDist ((y,m):xs) (x,n) | x == y = (x,n+m):xs
                              | otherwise = (y,m) : addDist xs (x,n)

instance Eq a => Monoid (Dist a) where
 mempty = Dist []

fromList :: (Foldable t, Eq a) => t a -> Dist a
fromList = foldMap pure

clean (Dist d) = foldMap (\(x,n) -> if n == 0 then mempty else Dist [(x,n)]) d

uniform xs = Dist $ (,1) <$> xs
dice n = uniform [1..n]
coin = uniform [0,1]
mult n (Dist d) = Dist $ fmap (* n) <$> d

instance Applicative Dist where
  pure x = Dist [(x, 1)]
  (<*>) = ap

instance Monad Dist where
  d >>= f = join $ f <$> d
    where join (Dist ds) = Dist $ ds >>= \(Dist d, n) -> fmap (* n) <$> d


          
          
