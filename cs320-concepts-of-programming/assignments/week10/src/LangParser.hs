module LangParser where

import Lang
import ParserMonad
import EnvUnsafe

keywords = ["if","then","else", "let", "in", "true","false"]


boolean = ["true", "false"]


parser :: Parser Ast
parser = apps

atoms :: Parser Ast
atoms = ints <|> bools <|> nil <|>  vars <|> lambdaParser <|> letParser <|> ifParser <|> parens

ints :: Parser Ast
ints = do s <- token $ intParser
          return $ ValInt s

bools :: Parser Ast
bools = do x <- token $ varParser
           if (x `elem` boolean)
            then (if x == "true" 
                   then return (ValBool True) 
                   else return (ValBool False))
            else failParse

nil :: Parser Ast
nil = do token $ literal "["
         token $ literal "]"
         return Nil

cons :: Parser Ast
cons = (do x <- orExpr
           token $ literal ":"
           x' <- token cons
           return (Cons x x')) <|> orExpr

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

apps :: Parser Ast
apps = withInfix cons [("",App)]

lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  x <- varParser
                  token $ literal "->"
                  y <- parser
                  return (Lam x y)

letParser :: Parser Ast
letParser = do token $ literal "let"
               x <- varParser
               token $ literal "="
               y <- parser
               token $ literal "in"
               z <- parser
               return (Let x y z)

ifParser :: Parser Ast
ifParser = do token $ literal "if"
              x <- parser
              token $ literal "then"
              y <- parser
              token $ literal "else"
              z <- parser
              return (If x y z)

parens :: Parser Ast
parens = do token $ literal "("
            x <- parser
            token $ literal ")"
            return x

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

andExpr :: Parser Ast
andExpr = withInfix addSubExpr [("&&", And)]

notExp :: Parser Ast
notExp = (do token $ literal "!"
             a <- notExp
             return $ Not a) <|> atoms

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

multDivExpr :: Parser Ast
multDivExpr = withInfix notExp [("*", Mult),("/", Div)]




data LangOut = ParseError | RuntimeError String | Result Val deriving Show

exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     Ok v -> Result v
                     Error e -> RuntimeError e
  _  -> ParseError