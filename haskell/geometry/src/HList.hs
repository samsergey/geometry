{-# LANGUAGE GADTs #-} 
class Someable a where
  some :: a -> String

instance Someable Int where
  some n = show n

data SomeString = SomeString String

instance Someable SomeString where
  some (SomeString s) = s

data SomeGroup where 
    Nil :: SomeGroup
    Cons :: Someable a => a -> SomeGroup -> SomeGroup

infixr 5 %:
a %: b = Cons a b

instance Someable SomeGroup where
    some Nil = ""
    some (Cons x Nil) = some x
    some (Cons x xs) = some x ++ ", " ++ some xs

list = Cons (3::Int) (Cons (SomeString "abc") (Cons (42::Int) Nil))
main = print . some $ list
