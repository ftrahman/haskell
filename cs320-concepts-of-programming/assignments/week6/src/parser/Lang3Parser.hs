module Lang3Parser where

import Data.Map (Map)-- for state
import qualified Data.Map as Map -- for State in tests

import Lang3 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang3
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")

literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` \ i -> LiteralInt i

plusParser :: Parser Ast
plusParser = (token (literal "(")) +++ parser +++ (token (literal "+")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Plus l r

seperatorParser :: Parser Ast
seperatorParser = (token (literal "(")) +++ parser +++ (token (literal ";")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Separator l r

assignParser :: Parser Ast
assignParser = (token (literal "(")) +++ varParser +++ (token (literal ":=")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, s), _), b), _) -> Assign s b


parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` (\ ((_,ast),_) -> ast)

idParser :: Parser Ast
idParser = token (varParser)
  `mapParser` (\ (i)  -> Var i)


parser :: Parser Ast
parser = literalIntParser <||> plusParser <||> seperatorParser <||> assignParser <||> parseParens <||> idParser
  `mapParser` \ e -> (case e of
    Left (Left (Left (Left (Left i)))) -> i
    Left (Left (Left (Left (Right i)))) -> i
    Left (Left (Left (Right i))) -> i
    Left (Left (Right i)) -> i
    Left (Right i) -> i
    Right i -> i)


    
-- for repl testing, will only work if you implement Lang3's eval
data Lang3Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean state keeping only the result
exec :: String -> Lang3Out
exec s = case parser s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i, _) -> Result i
    _  -> RuntimeError
  _  -> ParseError
