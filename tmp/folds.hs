import Text.Read
import Control.Monad

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
