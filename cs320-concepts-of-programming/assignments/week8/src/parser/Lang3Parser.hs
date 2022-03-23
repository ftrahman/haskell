module Lang3Parser where

import Data.Map (Map)-- for state
import qualified Data.Map as Map -- for State in tests

import State
import Lang3 (Ast(..), eval)
import ParserMonad


factor = assigns <|> vars <|> parseAstInt <|> parens 

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


mults :: Ast -> Parser Ast
mults left = 
  do s <- (token $ literal "*")
     ast <- factor
     let res =  left `Mult` ast
     (mults res) <|> return res

addSubs :: Ast -> Parser Ast
addSubs left = 
  do s <- token ((literal "+") <||> (literal "-"))
     ast <- term
     let res = case s of
                 Left _  -> left `Plus` ast
                 Right _ -> left `Sub` ast
     (addSubs res) <|> return res

seps :: Ast -> Parser Ast
seps left = 
  do s <- (token $ literal ";")
     ast <- sentence
     let res =  left `Separator` ast
     (seps res) <|> return res

vars :: Parser Ast
vars = token (varParser)
  `mapParser` (\ (i)  -> Var i)


assigns :: Parser Ast
assigns = 
  do s <- varParser     
     (token $ literal ":=")
     v <- sentence
     let res = Assign s v
     return res     

exAddSubs1 =  parse (addSubs (LiteralInt 6)) "  -  3"
exAddSubs2 =  parse (addSubs (LiteralInt 100)) " - 6 - 4"
exAddSubs3 =  parse (addSubs (LiteralInt 1)) "  + 10 - 5 + 4 -2"



-- for repl testing
data Lang3Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean state keeping only the result
exec :: String -> Lang3Out
exec s = case (parse parser) s of
  Just (ast,"") -> case runState (eval ast) Map.empty of
    (i, _) -> Result i
  _  -> ParseError
