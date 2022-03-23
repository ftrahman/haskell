module Lang1Parser where

import Lang1 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang1
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")
literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` \ i -> LiteralInt i

plusParser :: Parser Ast
plusParser = (token (literal "(")) +++ parser +++ (token (literal "+")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Plus l r

divParser :: Parser Ast
divParser = (token (literal "(")) +++ parser +++ (token (literal "/")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Div l r

-- parse the pattern "(x)" into "x"
parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` (\ ((_,ast),_) -> ast)
  
parser :: Parser Ast
parser = plusParser <||> divParser <||> literalIntParser <||> parseParens
  `mapParser` (\ e -> case e of
    Right i -> i
    Left (Right i) -> i
    Left (Left (Left i)) -> i
    Left (Left (Right i)) -> i) 



-- for repl testing, will only work if you implement Lang1's eval
data Lang1Out = ParseError | DivZeroErro | Result Integer deriving (Show, Eq)

exec :: String -> Lang1Out
exec s = case parser s of
  Just (ast,"") -> case eval ast of
    Just i -> Result i
    _      -> DivZeroErro
  _  -> ParseError
