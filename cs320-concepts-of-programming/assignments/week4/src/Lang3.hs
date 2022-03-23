module Lang3 where

-- we will use the standard Map
import Data.Map (Map)-- for state
import qualified Data.Map as Map

-- We will now add identifiers and a global assignment command to the abstract syntax tree. 
-- Assignment should evaluate to the value of the assignment and store the value in the global memory state.
-- The state (containing values for variables) is passed along as the evaluation proceeds; as Assign
-- expressions are evaluated, bindings are added to the state, and when Id expressions are evaluated
-- they are looked up in the state. Imagine walking around the AST in preorder and keeping track
-- of the state as we do so. 

data Ast =
      LiteralInt Integer
    | Id String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast


type State = Map String Integer

sumMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
sumMaybe Nothing Nothing = Nothing
sumMaybe Nothing (Just x) = Just x
sumMaybe (Just x) Nothing = Just x
sumMaybe (Just x) (Just y) = Just (x + y)

eval :: Ast -> State -> (Maybe Integer, State)
m0 = Map.empty
eval (LiteralInt x) m = (Just x, m)
eval (Id s) m = ((Map.lookup s m), m)
eval (Assign s ast) m =
  case (ast) of (LiteralInt x) -> (Just x, (Map.insert s x m))
                (Id s) -> (Nothing, m)
                (Plus ast1 ast2) -> (eval (Plus ast1 ast2) m)
                (Assign s ast) -> (eval (Assign s ast) m)
                (Separator ast1 ast2) -> (eval (Separator ast1 ast2) m)
eval (Plus ast1 ast2) m =
  case (eval ast1 m) of (x, m1) -> case (eval ast2 m) of (y, m2) -> ((sumMaybe x y), Map.union (Map.union m1 m2) m)
eval (Separator ast1 ast2) m =
  case (eval ast1 m) of (x, m1) -> case (eval ast2 m) of (y, m2) -> (y, Map.union (Map.union m1 m2) m)


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Id s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Assign s b) =  "(" ++ s ++ " := " ++ show b ++ ")"
