module Lang4Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests

import Reader
import Lang4 (Ast(..), eval)

import ParserMonad


factor = lets <|> vars <|> parseAstInt <|> parens 

term :: Parser Ast
term = 
  do l <- factor 
     mults l <|> return l  

sentence :: Parser Ast
sentence = 
  do l <- term
     addSub l <|> return l 

parser :: Parser Ast
parser = sentence

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)


addSub :: Ast -> Parser Ast
addSub left = do s <- token $ (literal "+") <||> (literal "-")
                 exp <- term
                 let res = case s of
                             Left _  -> left `Plus` exp
                             Right _ -> left `Sub` exp
                 (addSub res) <|> return res


mults :: Ast -> Parser Ast
mults left = 
  do s <- (token $ literal "*")
     exp <- factor
     let res =  left `Mult` exp
     (mults res) <|> return res

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

vars :: Parser Ast
vars = token (varParser)
  `mapParser` (\ (i)  -> Var i)

lets :: Parser Ast
lets = do token $ literal "let"
          x <- varParser
          token $ literal "="
          y <- sentence
          token $ literal "in"
          z <- sentence
          let res = Let x y z
          return res





-- for repl testing
data Lang4Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean env keeping only the result
exec :: String -> Lang4Out
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ runReader (eval ast) Map.empty
  _  -> ParseError



