{-# language DeriveTraversable #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}

import Text.Read
import Control.Monad
import Data.Monoid
import Data.List

type Program = String
type Stack = [Double]
type Command = String

calcRPN :: Program -> Stack -> Either String Stack
calcRPN prog s = interprete s (lexer prog)

lexer :: Program -> [Command]
lexer = words

interprete :: Stack -> [Command] -> Either String Stack
interprete = foldM (flip runCMD)

runCMD :: Command -> Stack -> Either String Stack
runCMD "+" = binop "+" (+) 
runCMD "-" = binop "-" (-) 
runCMD "*" = binop "*" (*) 
runCMD "/" = binop "/" (/) 
runCMD "n" = unop "negation" negate 
runCMD n = \s -> case readMaybe n of
    Nothing -> Left $ n <> " not a number"
    Just x -> Right $ x:s

binop cmd op (x : y : s) = Right $ y `op` x : s 
binop cmd _ [x] = Left $ cmd <> ": expected two args, got one: " <> show x
binop cmd _ [] = Left $ cmd <> ": expected two args, got none."

unop cmd op (x : s) = Right $ op x : s 
unop cmd _ [] = Left $ cmd <> ": expected one args, got none."

data T a = L | N (T a) a (T a)

quicksort :: Ord a => [a] -> [a]
quicksort = (`appEndo` []) . fold . unfold
  where unfold [] = L
        unfold (x:xs) = N (unfold [y | y <- xs , y <= x])
                          x
                          (unfold [y | y <- xs , y > x])

        fold L = mempty
        fold (N l x r) = fold l <> Endo ([x]++) <> fold r

longlist n = foldMap show [0..n]

