module Lang4 where

-- we will use the standard Map
import Data.Map (Map)-- for Env
import qualified Data.Map as Map

-- We will now add let expressions to the abstract syntax tree. 
-- Let String Ast Ast makes a local assignment of the first Ast to the String
-- in the state and evaluates the second Ast in this environment and returns
-- the result of the second Ast. 

data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Let String Ast Ast

type Env = Map String Integer

eval :: Ast -> Env -> Maybe Integer
e0 = Map.empty
eval (LiteralInt x) e = Just x
eval (Var s) e = (Map.lookup s e)
eval (Plus ast1 ast2) e =
  case (eval ast1 e) of (x) -> case (eval ast2 e) of (y) -> (sumMaybe x y)
--eval (Let s ast1 ast2) e =
  --case (eval ast1 e) of (x) ->
eval (Let s ast1 ast2) e =
  case (ast1) of (LiteralInt x) -> case (eval ast2 (Map.insert s x e)) of (y) -> (y)
                 (Var s) -> case (eval ast1 e) of (y) -> y
                 (Plus (LiteralInt x) (LiteralInt y)) -> case (eval ast2 (Map.insert s (x+y) e)) of y -> (y)
                 --(Plus x y) -> case (eval (Plus x y) e) of (x) -> case (eval ast2 (Map.insert s (maybeToInt x) e)) of y -> y
                 (Plus x y) -> case (eval x e) of (Nothing) -> Nothing
                                                  (x) -> case (eval y e) of (Nothing) -> Nothing
                                                                            (y) -> case (eval ast2 (Map.insert s ((maybeToInt x) + (maybeToInt y)) e)) of z -> z
                 (Let t ast3 ast4) -> case (eval ast3 e) of (x) -> case (eval ast4 (Map.insert t (maybeToInt x) e)) of y -> case (eval ast2 (Map.insert s (maybeToInt y) e)) of z -> z
                 --(Plus x y) -> case (eval )

sumMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
sumMaybe Nothing Nothing = Nothing
sumMaybe Nothing (Just x) = Just x
sumMaybe (Just x) Nothing = Just x
sumMaybe (Just x) (Just y) = Just (x + y)
-- show the fully parenthesized syntax
maybeToInt :: Maybe Integer -> Integer
maybeToInt (Just x) = x



-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Let s val inThis) =  "(let " ++ s ++ " = " ++ show val ++ " in " ++ show inThis ++ ")"
