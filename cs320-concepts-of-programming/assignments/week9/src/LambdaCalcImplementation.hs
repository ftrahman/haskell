module LambdaCalcImplementation where

import Data.Set (Set)
import qualified Data.Set as Set

data Term = FreeVar String | App Term Term | Lam String Term

-- 3 helper functions for the graders

-- makes a free var from a string
mkFreeVar :: String -> Term
mkFreeVar = FreeVar

-- applies the 2nd term to the first
mkApp  ::  Term -> Term -> Term
mkApp = App

-- creates a lambda out of a term and the name of a free var
bindToLam :: String -> Term -> Term
bindToLam = Lam


-- true if there no evaluation that can be done in the lambda expression
isValue :: Term -> Bool
isValue (FreeVar s) = True
isValue (App a b) = case a of (Lam c d) -> False
                              (App c d) -> case b of (Lam _ _) -> isValue (App c d)
                                                     (App e f) -> isValue (App c d) && isValue (App e f)
                                                     FreeVar _ -> isValue (App c d)
                              (FreeVar _) -> case b of (Lam _ _) -> True
                                                       (App c d) -> isValue (App c d)
                                                       _ -> True
isValue (Lam a b) = case b of (Lam c d) -> isValue (Lam c d)
                              (App c d) -> isValue (App c d)
                              (FreeVar _) -> True

ex = Lam "x" $ FreeVar "x" `App` (App (FreeVar "z") (FreeVar "x"))

-- collect all the vars that appear in the expression that are not bound
freeVars :: Term -> Set String
freeVars (FreeVar a) = Set.insert a Set.empty
freeVars (App a b) = freeVars a `Set.union` freeVars b
--freeVars (App a b) = freeVars a `Set.union` freeVars b
freeVars (Lam s t) = Set.delete s $ freeVars t




-- when are there no free variables in a lambda expression?
isClosed :: Term -> Bool
isClosed (FreeVar _) = False
isClosed x = if (freeVars x) == Set.empty then True else False


-- do all possible applications, rename bound variables as needed
eval :: Term -> Term
eval (FreeVar a) = (FreeVar a)
--eval (Lam a b) = Lam a b
--eval (App (Lam x e) v) = subst e x v
--eval (App (App e1 e2) v) =
--  let e = eval (App e1 e2) in (App e v)
eval (App t1 t2) = case (eval t1) of
    (Lam s t) -> (eval (subst t s t2 (Set.union (freeVars t1) (freeVars t2))))
    t1' -> (App t1' (eval t2))
eval (Lam s t) = (Lam s (eval t))


subst :: Term -> String -> Term -> Set String -> Term
subst (App f a)          from to avoid             = App (subst f from to avoid) (subst a from to avoid)
subst (FreeVar v)        from to _                 | v == from = to
                                                   | otherwise = FreeVar v
subst (Lam v bod)        from to avoid             | v == from = Lam v bod
                                                   | otherwise =
  let v' = findName v avoid
      bod' = rename bod v v'
  in Lam v' $ subst bod' from to (Set.insert v' avoid)

findName :: String -> Set String -> String
findName str avoidNames | Set.member str avoidNames = findName (str ++ "'") avoidNames
                        | otherwise                 = str
                        
boundVars :: Term -> Set String
boundVars (Lam v bod) = Set.insert v $ boundVars bod
boundVars (FreeVar _) = Set.empty
boundVars (App f a) = boundVars f `Set.union` boundVars a

rename :: Term -> String -> String -> Term
rename (App f a)      from to             = App (rename f from to) (rename a from to)
rename (Lam v bod)    from to | v == from = Lam v bod
rename (Lam v bod)    from to | otherwise = Lam v $ rename bod from to
rename (FreeVar v)    from to | v == from = FreeVar to
                              | otherwise = FreeVar v
-- show the unevaluated expression, in a human readable way.  It must be compatible with your parser.
-- you may choose a to show the fully parenthesized version

instance Show Term where
  show (FreeVar v) = v
  show (Lam s t) = "\\" ++ s ++ "." ++ show t
  show (App a b) = show a ++ " " ++ show b
-- equality on the structure of the term, where the names of bindings don't matter.
-- this is called alpha-equality

instance Eq Term where
  (FreeVar x) == (FreeVar y) = (x == y)
  (App a b) == (App c d) = (a == c) && (b == d)
  (Lam s t) == (Lam m n) =
    let determineName = findName m (Set.union (Set.union (freeVars t) (freeVars n)) (Set.union (boundVars t) (boundVars n)))
        lam1NewName = (rename t s determineName)
        lam2NewName = (rename n m determineName)
    in lam1NewName == lam2NewName
  a == b = False
