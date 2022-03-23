module Lang2Parser where

import Lang2 (Ast(..), eval)
import PrinterMonad(runPrinterMonad)
import ParserMonad



-- "S" in the grammar

factor = prints <|> parseAstInt <|> parens 

--phrase :: Parser Ast
--phrase = printsParser <|> factor

term :: Parser Ast
term = 
  do l <- factor 
     mults l <|> return l  

sentence :: Parser Ast
sentence = 
  do l <- term
     addSubs l <|> return l 

expresion :: Parser Ast
expresion = 
  do l <- sentence
     seps l <|> return l

parser :: Parser Ast
parser = expresion



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


prints :: Parser Ast
prints = 
  do token $ literal "print("
     ast <- expresion
     token $ literal ")"
     return (Print ast)
     

-- "F" in the grammar, could also call it parensOrInt

-- we need to deal with the left recursive case of "T" together
--	T -> T * F
--  T -> T / F
--  T -> F

-- this will parse all the top level multiplication and division, given what was parsed on the left
mults :: Ast -> Parser Ast
mults left = 
  do s <- (token $ literal "*")
     ast <- factor
     let res =  left `Mult` ast
     (mults res) <|> return res
 
ex4 = parse (mults (LiteralInt 70)) "* 9  "
ex5 = parse (mults (LiteralInt 7)) " * 9 * 8 * 44 "
ex6 = parse (mults (LiteralInt 0)) " * 0  "


-- finally we combine all case of "T" together
--	T -> T * F
--  T -> T / F
--  T -> F

-- run these in your repl to test


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

exAddSubs1 =  parse (addSubs (LiteralInt 6)) "  -  3"
exAddSubs2 =  parse (addSubs (LiteralInt 100)) " - 6 - 4"
exAddSubs3 =  parse (addSubs (LiteralInt 1)) "  + 10 - 5 + 4 -2"

seps :: Ast -> Parser Ast
seps left = 
  do s <- (token $ literal ";")
     ast <- sentence
     let res =  left `Separator` ast
     (seps res) <|> return res

ex = parse (seps (LiteralInt 70)) " ; 9  "
ex2 = parse (seps (LiteralInt 7)) " ; 9 ; 8 + 44 "
ex3 = parse (seps (LiteralInt 0)) " ; 0  "




-- finally we combine all case of "E" together
--  E -> E + T
--  E -> E - T
--  E -> T


-- for repl testing
data Lang2Out = ParseError | Result Integer [Integer] deriving (Show, Eq)

exec :: String -> Lang2Out
exec s = case (parse parser) s of
  Just (ast,"") -> case runPrinterMonad (eval ast) of
    (ls,i) -> Result i ls
  _  -> ParseError
