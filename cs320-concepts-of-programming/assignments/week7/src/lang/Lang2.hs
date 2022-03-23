module Lang2 where

import PrinterMonad

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast
  | Sub Ast Ast
  | Mult Ast Ast


eval :: Ast -> PrinterMonad Integer Integer
eval (LiteralInt i) = return i
eval (Print x) = 
  do x' <- eval x
     printThis x'
eval (x `Plus` y) =
  do x' <- eval x
     y' <- eval y
     return (x' + y')
eval (x `Sub` y) =
  do x' <- eval x
     y' <- eval y
     return (x' - y')
eval (x `Mult` y) =
  do x' <- eval x
     y' <- eval y
     return (x' * y')
eval (x `Separator` y) = 
  do x' <- eval x
     y' <- eval y
     return y'

printThis :: Integer -> PrinterMonad Integer Integer
printThis x = PrinterMonad [x] x



  

-- Ungraded bonus: There is a built in monad on tuples with the same functionality
eval' :: Ast -> ([Integer], Integer)
eval' =  undefined

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Print b) =  "print(" ++ show b ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
