module Lang2Parser where

import Lang2 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang2
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")
literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` \ i -> LiteralInt i

plusParser :: Parser Ast
plusParser = (token (literal "(")) +++ parser +++ (token (literal "+")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Plus l r

seperatorParser :: Parser Ast
seperatorParser = (token (literal "(")) +++ parser +++ (token (literal ";")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((((_, l), _), r), _) -> Seperator l r

printParser :: Parser Ast
printParser = (token (literal "print(")) +++ parser +++ (token (literal ")"))
  `mapParser` \ ((_, i)_)) -> Print i 

-- parse the pattern "(x)" into "x"
parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` \ ((_, i), _) -> i
  
parser :: Parser Ast
parser = plusParser <||> seperatorParser <||> printParser <||> literalIntParser <||> parseParens
  `mapParser` \ e -> case e of
    Right i -> i
    Left (Right i) -> i
    Left (Left (Right i)) -> i
    Left (Left (Left (Right i)) -> i)
    Left (Left (Left (Left i)) -> i)  

-- for repl testing, will only work if you implement Lang1's eval
data Lang2Out = ParseError | Result Integer [Integer] deriving (Show, Eq)

exec :: String -> Lang2Out
exec s = case parser s of
  Just (ast,"") -> case eval ast of
    (ls,i) -> Result i ls
  _  -> ParseError
