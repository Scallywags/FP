{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module FPProj3 where

import Data.List
import Data.Maybe
import Data.Char

{- ===============================================================

                         README
                            
      To evaluate an expression you are given two options:
         
      1. Call evalMulti (yourProgram) (yourQuery)
         Example:    evalMulti program [Atom "p", (Var "X")]
            
      2. Call parse (yourQueryAsAStringInPrologNotation)
         Example:    parse "r(X)"    (See parser for more info)

================================================================-}



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



-- =========== Expression type class ===========

class Expr e where
    (<=~) :: e -> Substitution -> e

instance Expr Term where
    (Var x)         <=~     subs     = case find (\(Var y, repl) -> y == x) subs of
        Just (_, repl)  -> repl
        Nothing         -> Var x

    (Const x)       <=~     _        = Const x

instance Expr Atom where
    (Atom p ts)     <=~     subs     = Atom p (map (<=~ subs) ts)

instance Expr Clause where
    (a, as)         <=~     subs     = (a <=~ subs, map (<=~ subs) as)

instance Expr Program where
    prog            <=~     subs     = map (<=~ subs) prog

    
-- =========== Helper functions ==========

val :: Term -> String
val (Var x)   = x
val (Const x) = x

value :: Atom -> [String]
value (Atom _ terms) = [val t | t <- terms]

predicate :: Atom -> Predicate
predicate (Atom p _)    = p



-- =========== Unify ===========

unify :: Atom -> Atom -> Maybe Substitution
unify a1 a2     = unify' [] a1 a2

unify' :: Substitution -> Atom -> Atom -> Maybe Substitution
unify' subs     (Atom p1 (t1@(Var x):terms1))   (Atom p2 (t2:terms2))           | p1 == p2              = unify' (sub:subs) (Atom p1 newTerms1) (Atom p2 newTerms2)
    where
        sub = (t1, t2)
        newTerms1 = map (<=~ [sub]) terms1
        newTerms2 = map (<=~ [sub]) terms2
unify' subs     (Atom p1 (t1@(Const x):terms1)) (Atom p2 (t2@(Var y):terms2))   | p1 == p2              = unify' (sub:subs) (Atom p1 newTerms1) (Atom p2 newTerms2)
    where
        sub = (t2, t1)
        newTerms1 = map (<=~ [sub]) terms1
        newTerms2 = map (<=~ [sub]) terms2
unify' subs     (Atom p1 ((Const x):terms1))    (Atom p2 ((Const y):terms2))    | p1 == p2 && x == y    = unify' subs (Atom p1 terms1)  (Atom p2 terms2)
unify' subs     (Atom p1 [])                    (Atom p2 [])                    | p1 == p2              = Just subs
unify' _        _                               _                               = Nothing



-- =========== Rename ===========

rename :: Query -> Clause -> Clause
rename query clause = foldr rename' clause query

rename' :: Atom -> Clause -> Clause
rename' (Atom q qts) c@(Atom p ts, _)
    | p == q    = c <=~ subs
    | otherwise = c                                         
    where
        eligableNames = filter (not . (`elem` (map val qts))) names
        subs = zip ts (map Var eligableNames)

names :: [String]
names = concat $ iterate (zipWith (++) aTOz) aTOz
    where aTOz = map (:[]) ['A'..'Z']

    
    
-- =========== Evaluate ===========

evalMulti :: Program -> Query -> Solution
evalMulti prog query    = case evalMulti' prog query of
                            Just subs   -> (True, subs)
                            Nothing     -> (False, [])

evalMulti' :: Program -> Query -> Maybe [Substitution]
evalMulti' _        []      = Just []
evalMulti' prog     (q:qs)  | subsMaybe == Nothing    = Nothing
                            | otherwise               = Just (sub : fromJust (evalMulti' prog qs))
    where
        subsMaybe = findSubs prog q prog
        sub = fromJust subsMaybe

findSubs :: Program -> Atom -> Program -> Maybe Substitution
findSubs pro (Atom _ [])                 _                      = Just []
findSubs pro _                           []                     = Nothing
findSubs pro q@(Atom p ((Const x):ts))   prog@(c@(a, as):cs)    = findSubs pro (Atom p ts) prog
findSubs pro q@(Atom p ((Var x):ts))     prog@(c@(a, as):cs)    = Just (sub ++ fromJust (findSubs pro (Atom p ts) cs))
    where
        unificMaybe = unify q a
        sub = case unificMaybe of
            Just subs   -> subs
            Nothing     -> []

            
         
-- ============= Parser =============

data State = Q0 | Q1

-- This parser accepts string in prolog notation up to a certain degree.
-- predicates and terms can be words
-- Constants start with a lowercase letter
-- Variables start with an uppercase letter or an underscore (_)
-- Example: r(X, Y)
--          mother(tess, _)
--          functionWithLotsOfVariablesAndConstants(VarA, VarB, VarC, VarD, constA, constB, VarE, constC)
parse :: String -> Atom
parse ps = Atom name terms
    where
        (name, '(':rest) = parseName  [] ps
        terms            = parseTerms Q0 rest []

parseTerms :: State -> String -> [Term] -> [Term]
parseTerms Q0 (p:ps)   ts | isLower p               = parseTerms Q1 rest (ts ++ [Const name])
                          | isUpper p || p == '_'   = parseTerms Q1 rest (ts ++ [Var   name])
    where
        (name, rest) = parseName [] (p:ps)
parseTerms Q1 (p:ps)   ts | p == ','  = parseTerms Q0 ps ts
                          | p == ')'  = ts
parseTerms s  (' ':ps) ts             = parseTerms s ps ts
parseTerms _  (p:ps)   _              = error ("Parse error on token " ++ [p])
parseTerms _  _        _              = error "Parse error"


parseName :: String -> String -> (String, String)
parseName rs (s:ss) | isLetter s || s == '_' = parseName (rs ++ [s]) ss
                    | otherwise              = (rs, (s:ss))

parseClause :: (String, [String]) -> Clause
parseClause (s, ss) = (parse s, map parse ss)
                    
                    
                    
-- =========== Test Program ===========

statements :: [(String, [String])]
statements = [("p(a)", [])
             ,("p(b)", [])
             ,("p(c)", [])
             ,("q(a)", [])
             ,("q(b)", [])
             
             ,("r(X)", ["q(X)"])
             ,("s(X)", ["p(X)", "q(X)"])
             ,("t(X)", ["p(X)", "q(b)"])
             ,("t(X)", ["p(a)", "p(b)"])
             ,("u(X)", ["p(c)", "r(X)"])
             ,("v(a)", ["q(X)", "p(c)", "r(X)", "s(X)", "q(b)"])
             ,("w(Y)", ["r(Y)"])
             ,("r(A)", ["p(Y)"])
             
             ,("mother(anna, june)", [])
             ,("mother(philip, anna)", [])
             ,("isMotherAndHasChild(X, Y)", ["mother(Y,X)"])
             ,("aaa(bbb, CCC)", [])
             ,("aaa(X, bbb)", ["q(X)"])
             ]

familyStatements :: [(String, [String])]
familyStatements = [("mother(emma,wilhelmina)", [])
                   ,("mother(wilhelmina,juliana)", [])
                   ,("mother(juliana,beatrix)", [])
                   ,("mother(juliana,margriet)", [])
                   ,("mother(juliana,irene)", [])
                   ,("mother(juliana,christina)", [])
                   ,("mother(margriet,maurits)", [])
                   ,("mother(margriet,bernhard_jr)", [])
                   ,("mother(margriet,pieter_christiaan)", [])
                   ,("mother(margriet,floris)", [])
                   ,("mother(beatrix,alexander)", [])
                   ,("mother(beatrix,friso)", [])
                   ,("mother(beatrix,constantijn)", [])
                   ,("mother(maxima,amalia)", [])
                   ,("mother(maxima,alexia)", [])
                   ,("mother(maxima,ariane)", [])

                   ,("husband(bernhard,juliana)", [])
                   ,("husband(claus,beatrix)", [])
                   ,("husband(pieter,margriet)", [])
                   ,("husband(alexander,maxima)", [])
                   ,("husband(friso,mabel)", [])
                   ,("husband(constantijn,laurentien)", [])

                   ,("female(irene)", [])
                   ,("female(christina)", [])
                   ,("female(amalia)", [])
                   ,("female(alexia)", [])
                   ,("female(ariane)", [])
                   ,("female(X)", ["mother(X,_)"])
                   ,("female(X)", ["husband(_,X)"])

                   ,("male(maurits)", [])
                   ,("male(bernhard_jr)", [])
                   ,("male(pieter_christiaan)", [])
                   ,("male(floris)", [])
                   ,("male(X)", ["husband(X,_)"])
                   ]
                   
smallStatements :: [(String, [String])]
smallStatements = [("p(a)", [])
                  ,("p(b)", [])
                  ,("p(c)", [])
                  ,("q(a)", [])
                  ,("q(b)", [])
                  ,("r(X)", ["p(X)"])
                  ,("s(X)", ["p(X)", "q(X)"])
                  ]
                    
program :: Program
program = map parseClause statements

familyProgram :: Program
familyProgram = map parseClause familyStatements

smallProgram :: Program
smallProgram = map parseClause smallStatements



-- ============= Tests =============

familyQueries :: [Query]
familyQueries = [ [parse "mother(maxima, ariane)"]
                , [parse "mother(X, floris)"]
                , [parse "mother(margriet, A)"]
                , [parse "husband(pieter, margriet)"]
                , [parse "female(christina)"]
                , [parse "female(maxima)"]
                , [parse "male(X)"]
                , [parse "husband(X, Y)"]
                , [parse "male(alexander)"]
                ]

familyTest :: Bool
familyTest = (and $ map (fst) $ map (evalMulti familyProgram) familyQueries)