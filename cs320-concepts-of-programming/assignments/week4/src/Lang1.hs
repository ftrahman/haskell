module Lang1 where

-- We have added division to the Ast.  Division in the language should act like Haskell's `div`.

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Div Ast Ast

-- Evaluate the AST to calculate its value
-- ex: eval (1 + (10 / 0)) + 2  ==> None
-- ex: eval (1 + (10 / 2)) + 2  ==> Just 8

sumMaybe :: (Maybe Integer, Maybe Integer) -> Maybe Integer
sumMaybe (Nothing, Nothing) = Nothing
sumMaybe (Nothing, Just x) = Just x
sumMaybe (Just x, Nothing) = Just x
sumMaybe (Just x, Just y) = Just (x + y)

divMaybe :: (Maybe Integer, Maybe Integer) -> Maybe Integer
--ivMaybe (_, 0) = Nothing
divMaybe (Nothing, Nothing) = Nothing
divMaybe (Nothing, Just x) = Just x
divMaybe (Just x, Nothing) = Just x
divMaybe (Just x, Just y) = Just (x `div` y)


eval :: Ast -> Maybe Integer
eval (LiteralInt x) = Just x
eval (Plus (LiteralInt x) (LiteralInt y)) = sumMaybe (Just x, Just y)
eval (Div (_) (LiteralInt 0)) = Nothing
eval (Div (LiteralInt x) (LiteralInt y)) = divMaybe (Just x, Just y)
eval (Plus (Plus x y) (LiteralInt a)) = (sumMaybe ((eval (Plus x y)),(Just a)))
eval (Plus (Div x y) (LiteralInt a)) = (sumMaybe ((eval (Div x y)),(Just a)))
eval (Div (Plus x y) (LiteralInt a)) = (divMaybe ((eval (Plus x y)),(Just a)))
eval (Div (Div x y) (LiteralInt a)) = (divMaybe ((eval (Div x y)), (Just a)))
eval (Plus (LiteralInt a) (Plus x y)) = (sumMaybe ((eval (Plus x y)),(Just a)))
eval (Div (LiteralInt a) (Plus x y)) = (divMaybe ((eval (Plus x y)),(Just a)))
eval (Plus (LiteralInt a) (Div x y)) = (sumMaybe ((eval (Div x y)),(Just a)))
eval (Div (LiteralInt a) (Div x y)) = (divMaybe ((eval (Div x y)),(Just a)))
eval (Plus (Plus x y) (Plus a b)) = (sumMaybe ((eval (Plus x y)), (eval (Plus a b))))
eval (Plus (Div x y) (Plus a b)) = (sumMaybe ((eval (Div x y)), (eval (Plus a b))))
eval (Plus (Plus x y) (Div a b)) = (sumMaybe ((eval (Plus x y)), (eval (Div a b))))
eval (Plus (Div x y) (Div a b)) = (sumMaybe ((eval (Div x y)), (eval (Div a b))))
eval (Div (Div x y) (Div a b)) = (divMaybe ((eval (Div x y)), (eval (Div a b))))
eval (Div (Plus x y) (Div a b)) = (divMaybe ((eval (Plus x y)), (eval (Div a b))))
eval (Div (Div x y) (Plus a b)) = (divMaybe ((eval (Div x y)), (eval (Plus a b))))
eval (Div (Plus x y) (Plus a b)) = (divMaybe ((eval (Plus x y)), (eval (Plus a b))))


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
