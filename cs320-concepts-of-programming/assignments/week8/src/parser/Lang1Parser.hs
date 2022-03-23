module Lang1Parser where

import Lang1 (Ast(..), eval)
import ParserMonad

-- Integers in the grammar
parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)

-- ( E ) in the grammar
parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

-- "F" in the grammar, could also call it parensOrInt


-- we need to deal with the left recursive case of "T" together
--	T -> T * F
--  T -> T / F
--  T -> F

-- this will parse all the top level multiplication and division, given what was parsed on the left
multDivs :: Ast -> Parser Ast
multDivs left = 
  do s <- token ((literal "*") <||> (literal "/"))
     ast <- factor
     let res = case s of
                 Left _  -> left `Mult` ast
                 Right _ -> left `Div` ast
     (multDivs res) <|> return res -- keep doing this as much as possible

-- run these in your repl to tests
exMultDivs1 =  parse (multDivs (LiteralInt 0)) "*6"
exMultDivs2 =  parse (multDivs (LiteralInt 6)) "  /  3"
exMultDivs3 =  parse (multDivs (LiteralInt 1)) "  * 10 / 5 * 4 /2"


 
-- finally we combine all case of "T" together
--	T -> T * F
--  T -> T / F
--  T -> F
	 
-- run these in your repl to test
exTerm1 =  parse term "100"
exTerm2 =  parse term "0*6"
exTerm3 =  parse term "6  /  3"
exTerm4 =  parse term " 1 * 10 / 5 * 4 /2"



-- we need to deal with the left recursive case of "E" together
--  E -> E + T
--  E -> E - T

addSubs :: Ast -> Parser Ast
addSubs left = 
  do s <- token ((literal "+") <||> (literal "-"))
     ast <- term
     let res = case s of
                 Left _  -> left `Plus` ast
                 Right _ -> left `Sub` ast
     (addSubs res) <|> return res

-- run these in your repl to test
exAddSubs1 =  parse (addSubs (LiteralInt 6)) "  -  3"
exAddSubs2 =  parse (addSubs (LiteralInt 100)) " - 6 - 4"
exAddSubs3 =  parse (addSubs (LiteralInt 1)) "  + 10 - 5 + 4 -2"


factor = parseAstInt <|> parens 

term :: Parser Ast
term = multOrDivOrParensOrInt <|> factor

multOrDivOrParensOrInt :: Parser Ast
multOrDivOrParensOrInt = 
  do l <- factor -- every term starts with a factor
     multDivs l


addOrSubOrMultOrDivOrParensOrInt :: Parser Ast
addOrSubOrMultOrDivOrParensOrInt =
  do l <- term
     addSubs l

-- finally we combine all case of "E" together
--  E -> E + T
--  E -> E - T
--  E -> T
expresion :: Parser Ast
expresion = addOrSubOrMultOrDivOrParensOrInt <|> term


-- "S" in the grammar
parser :: Parser Ast
parser = expresion





-- for repl testing
data Lang1Out = ParseError | DivZeroErro | Result Integer deriving (Show, Eq)

exec :: String -> Lang1Out
exec s = case (parse parser) s of
  Just (ast,"") -> case eval ast of
    Just i -> Result i
    _      -> DivZeroErro
  _  -> ParseError
