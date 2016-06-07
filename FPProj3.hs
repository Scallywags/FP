{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module FPProj3 where

import Data.List
import Data.Maybe
import Data.Char

-- =========== Types ===========

type Predicate = String

data Term   = Var String
            | Const String
            deriving (Show, Eq)

data Atom   = Atom Predicate [Term] deriving (Show, Eq)

type Clause = (Atom, [Atom])
type Query  = [Atom]

type Program    = [Clause]

type Substitution   = [(Term, Term)]

type Solution       = (Bool, [Substitution])

-- =========== Unify ===========

unify :: Atom -> Atom -> Substitution
unify a1 a2     = unify' [] a1 a2

unify' :: Substitution -> Atom -> Atom -> Substitution
unify' subs     (Atom p1 (t1@(Var x):terms1))   (Atom p2 (t2:terms2))           | p1 == p2              = unify' (sub:subs) (Atom p1 newTerms1) (Atom p2 newTerms2)
    where
        sub = (t1, t2)
        newTerms1 = map (<=~ sub) terms1
        newTerms2 = map (<=~ sub) terms2
unify' subs     (Atom p1 (t1@(Const x):terms1)) (Atom p2 (t2@(Var y):terms2))   | p1 == p2              = unify' (sub:subs) (Atom p1 newTerms1) (Atom p2 newTerms2)
    where
        sub = (t2, t1)
        newTerms1 = map (<=~ sub) terms1
        newTerms2 = map (<=~ sub) terms2
unify' subs     (Atom p1 ((Const x):terms1))    (Atom p2 ((Const y):terms2))    | p1 == p2 && x == y    = unify' subs (Atom p1 terms1)  (Atom p2 terms2)
unify' subs     (Atom p1 [])                    (Atom p2 [])                    | p1 == p2              = subs
unify' _        _                               _                               = []


class Expr e where
    (<=~) :: e -> (Term, Term) -> e

instance Expr Term where
    (Var x)         <=~     (Var y, replacement)    | x == y    = replacement
                                                    | otherwise = Var x
    (Var x)         <=~     (Const y, _)            = Var x
    (Const x)       <=~     _                       = Const x

instance Expr Atom where
    (Atom p ts)     <=~     sub     = Atom p (map (<=~ sub) ts)

instance Expr Clause where
    (a, as)         <=~     sub     = (a <=~ sub, map (<=~ sub) as)

instance Expr Program where
    prog            <=~     sub     = map (<=~ sub) prog

-- =========== Evaluate ===========

evalMulti :: Program -> Query -> Solution
evalMulti prog (q:qs) = (solvable, subs)   --TODO
    where
        solvable = False --TODO
        subs = [] --TODO




findSubstitutions :: Program -> Program -> Atom -> [Substitution]
findSubstitutions _     []                             _        = []
findSubstitutions prog  ((a@(Atom cpred cterms), as):cs)    q@(Atom qpred qterms)
    | cpred == qpred && length cterms == length qterms          = subs
    | otherwise = findSubstitutions prog cs q
    where
        ((t1, ts2):ts) = unify q a
        subs = case ts2 of
            Var x   ->  snd (evalMulti prog as) --TODO use ts?
            Const x ->  [] --TODO
            

            
            
-- ============= Parser =============

data State = Q0 | Q1

{-- TODO this will not work until evalMulti works. In the meantime we can use the function below.

parse :: String -> Solution
parse (p:ps) = evalMulti program [Atom name terms]
    where
        (name, '(':rest) = parseName [] (p:ps)
        terms            = parseTerms Q0 rest []
parse _      = error "Parse error in parse"
--}

parse :: String -> Atom
parse ps = Atom name terms
    where
        (name, '(':rest) = parseName  [] ps
        terms            = parseTerms Q0 rest []

parseTerms :: State -> String -> [Term] -> [Term]
parseTerms Q0 (p:ps)   ts | isLower p = parseTerms Q1 rest (ts ++ [Const name])
                          | isUpper p = parseTerms Q1 rest (ts ++ [Var   name])
    where
        (name, rest) = parseName [] (p:ps)
parseTerms Q1 (p:ps)   ts | p == ','  = parseTerms Q0 ps ts
                          | p == ')'  = ts
parseTerms s  (' ':ps) ts             = parseTerms s ps ts
parseTerms _  (p:ps)   _              = error ("Parse error on token " ++ [p])
parseTerms _  _        _              = error "Parse error"


parseName :: String -> String -> (String, String)
parseName rs (s:ss) | isLetter s = parseName (rs ++ [s]) ss
                    | otherwise  = (rs, (s:ss))
                    
                    
                    
                    
-- =========== Test Program ===========