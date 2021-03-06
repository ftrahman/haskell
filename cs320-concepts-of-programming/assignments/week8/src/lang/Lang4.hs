module Lang4 where

import Data.Map (Map)-- for Env
import qualified Data.Map as Map

import HelpShow

import Reader

data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Let String Ast Ast
    | Sub Ast Ast
    | Mult Ast Ast
  deriving Show
  
type Env = Map String Integer

-- for simplicity do not we will not separately encode failure at the type level,
-- you may return 0 for variable that are not defined

eval :: Ast -> Reader Env Integer
eval (LiteralInt i) = return i
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
eval (Var s) = 
  do r <- ask
     case Map.lookup s (r) of
        Just var -> return var
        Nothing -> return 0
eval (Let v val bod) = 
  do val' <- eval val
     local (Map.insert v val') (eval bod)

local :: (r -> r) -> Reader r a -> Reader r a
local changeEnv comp  = Reader (\e -> runReader comp (changeEnv e) )

-- Let String Ast Ast makes a local assignment of the first Ast to the String
-- in the state and evaluates the second Ast in this environment and returns
-- the result of the second Ast. 




eval' :: Ast -> Env -> Integer -- functions have are already defined with the Reader Monad
eval' = undefined -- Ungraded practice problem




-- show the fully parenthesized syntax
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i)     = show i
showFullyParen (Var s)            = s
showFullyParen (l `Plus` r)       = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Sub` r)        = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)       = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"
showFullyParen (Let s val inThis) =  "(let " ++ s ++ " = " ++ showFullyParen val ++ " in " ++ showFullyParen inThis ++ ")"


showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i)     _ = show i
showPretty (Var s)            _ = s
showPretty (l `Mult` r)       d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)       d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)        d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
showPretty (Let s val inThis) d = parenthesize d 5  ("let " ++ s ++ " = " ++ showPretty val 4 ++ " in " ++ showPretty inThis 5 ) -- binds most weakly

--instance Show Ast where
--  show e = showPretty 100
