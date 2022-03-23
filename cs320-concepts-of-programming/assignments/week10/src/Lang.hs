module Lang where

import Data.Map (Map)
import qualified Data.Map as Map

import HelpShow

import EnvUnsafe


-- Here is the abstract syntax tree for our language

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
           deriving (Eq,Show) -- helpful to use this during testing
         --deriving Eq

--instance Show Ast where
  --show ast = showPretty ast 0


-- the goal of the program is to return a value
data Val = I Integer | B Bool
         | Ls [Val]
         | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function


stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \v -> case v of Ls (h:_) -> Ok $ h
                                  --Ls ([])  -> Error "please stop"
                                  _        -> Error "so what? no head? *phone smash*"),
   ("len", Fun $ \v -> case v of Ls xs -> Ok $ I $ toInteger $ len xs
                                 _     -> Error "not very lengthy :/")]

-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> Unsafe Val
run a = runEnvUnsafe (eval a) stdLib

len :: [Val] -> Integer
len ([])  = 0
len (x:xs) = 1 + (len xs)


type Env = Map String Val


eval :: Ast -> EnvUnsafe Env Val
eval (ValBool b) = return (B b) 
eval (And x y) = 
  do xres <- evalBool x
     yres <- evalBool y
     return (B (xres && yres))
eval (Or x y) = 
  do xres <- evalBool x
     yres <- evalBool y
     return (B (xres || yres))
eval (Not x) = 
  do xres <- evalBool x
     return (B (not xres))
eval (ValInt i) = return (I i)
eval (Plus x y) =
  do xres <- evalInt x
     yres <- evalInt y
     return (I (xres + yres))
eval (Minus x y) = 
  do xres <- evalInt x
     yres <- evalInt y
     return (I (xres - yres))
eval (Mult x y) = 
  do xres <- evalInt x
     yres <- evalInt y
     return (I (xres * yres))
eval (Div x y) =
  do xres <- evalInt x
     yres <- evalInt y
     if yres == 0 then err "we are too divided" else return (I (xres `div` yres))
eval (Nil) = 
  return (Ls [])
eval (Cons x y) = 
  do x' <- eval x
     y' <- eval y
     case y' of Ls ls -> return $ Ls $ [x'] ++ ls
                _     -> err "ur such a con"
eval (If x y z) = 
  do xres <- eval x 
     case xres of  
          B b -> if b then eval y else eval z
          _ -> err "what if"
eval (Let v val bod) =
  do env <- getEnv
     val' <- eval val
     EnvUnsafe (\env -> runEnvUnsafe (eval bod) (Map.insert v val' env))
eval (Lam x bod) = 
  do env <- getEnv
     return $ Fun $ \  v ->  runEnvUnsafe (eval bod) (Map.insert x v env)
eval (Var s) = 
  do env <- getEnv
     case Map.lookup s (env) of
          Just var -> return var
          Nothing -> err "not the VARy cool"
eval (App x y) = 
  do x' <- evalFun x
     y' <- eval y
     case x' y' of (Ok a) -> return a
                   _ -> err "not hAPPy right now"
     --case x' of Fun f -> return $ fuckIt (f y')
       --         _ -> err "fuck"

evalList :: Ast -> EnvUnsafe Env [Val]
evalList a = 
  do res <- eval a
     case res of
          Ls ls -> return ls
          _ -> err "Not the ls we wanted </3"

evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a = 
  do res <- eval a
     case res of
          I i -> return i
          _ -> err "Not the int we wanted </3"

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = 
  do res <- eval a
     case res of
          B b -> return b
          _ -> err "Not the bool we deserved </3"

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a = 
  do res <- eval a
     case res of
          Fun f -> return f
          _ -> err "This isn't fun anymore"


-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"


-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)
showPretty (Mult l r) i = parenthesize 12 i $ (showPretty l 12) ++ " * " ++ (showPretty r 13)
showPretty (Div l r) i = parenthesize 12 i $ (showPretty l 12) ++ " / " ++ (showPretty r 13)

showPretty (Not l ) i = parenthesize 14 i $  " ! " ++ (showPretty l 14)


