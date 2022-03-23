module Lang0 where

-- Consider the language defined by the following AST (Abstract Syntax Tree) for adding numbers

data Ast =
    LiteralInt Integer
  | Plus Ast Ast

-- it can be evaluated with the function eval:
-- ex: eval ((1 + 3) + 2)  ==> 6

eval :: Ast -> Integer
eval (LiteralInt x) = x
eval (Plus (LiteralInt x) (LiteralInt y)) = x + y
eval (Plus (Plus x y) (LiteralInt a)) = (eval (Plus x y)) + a
eval (Plus (LiteralInt a) (Plus x y)) = (eval (Plus x y)) + a
eval (Plus (Plus x y) (Plus a b)) = (eval (Plus x y)) + (eval (Plus a b))


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
