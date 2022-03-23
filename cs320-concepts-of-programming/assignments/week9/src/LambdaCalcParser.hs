module LambdaCalcParser where
import LambdaCalcImplementation
import ParserMonad

parser :: Parser Term
parser = withApp

withApp = appParser <|> withLam
withLam = lamParser <|> justVar
justVar = freeVarParser <|> parens

appParser :: Parser Term
appParser =
  do left <- withLam
     parserHelper left <|> return left

parserHelper :: Term -> Parser Term
parserHelper left = do s <- (token (literal ".")) <||> (spaces)
                       right <- withLam
                       let res = App left right
                       parserHelper res <|> return res

lamParser :: Parser Term
lamParser = do s <- (token $ literal "\\")
               name <- token $ varParser
               token $ literal "->"
               term <- parser
               let res = Lam name term
               return res


freeVarParser :: Parser Term
freeVarParser = do name <- token $ varParser
                   let res = FreeVar name
                   return res

parens :: Parser Term
parens = do token $ literal "("
            term <- parser
            token $ literal ")"
            return term



-- ungraded bonus: also parse wild cards like _ so you can write ( \ _ -> x)
-- ungraded bonus: parse numbers into their church encodings

-- for repl testing
data LambdaOut = ParseError | Result Term deriving (Show, Eq)

exec :: String -> LambdaOut
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ eval ast
  _  -> ParseError
