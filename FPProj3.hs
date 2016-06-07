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
                            | otherwise               = Just (sub ++ fromJust (evalMulti' prog qs))
    where
        subsMaybe = findSubs q prog
        sub = fromJust subsMaybe

findSubs :: Atom -> Program -> Maybe [Substitution]
findSubs (Atom _ [])                 _                      = Just []
{-findSubs q@(Atom p ((Const x):ts))   prog@(c@(a, as):cs)    = findSubs ts prog
findSubs q@(Atom p ((Var x):ts))     prog@(c@(a, as):cs)    = Just (sub:fromJust (findSubs ts prog))
    where
        unificMaybe = unify q a
        sub = case unificMaybe of
            Just subs   -> subs
            Nothing     -> []

        matches = getMatches q prog
-}


getMatches :: Atom -> Program -> [[Clause]]
getMatches (Atom _ [])                     _    = []
getMatches q@(Atom qp qts@((Const x):ts))  cs   = getMatches (Atom qp ts) cs
getMatches q@(Atom qp qts@((Var x):ts))    cs   = matches : getMatches (Atom qp ts) cs
    where
        matches = [c | c@(a@(Atom cp cts), _) <- cs, qp == cp, length cts == length qts, unify q a /= Nothing]

-- =========== Example Program ===========

program :: Program
program     =   [(Atom "mother" [Const "emma", Const "wilhelmina"], [])
                ,(Atom "mother" [Const "wilhelmina", Const "juliana"], [])
                ,(Atom "mother" [Const "juliana", Const "beatrix"], [])
                ,(Atom "mother" [Const "juliana", Const "margriet"], [])
                ,(Atom "mother" [Const "juliana", Const "irene"], [])
                ,(Atom "mother" [Const "juliana", Const "christina"], [])
                ,(Atom "mother" [Const "margriet", Const "maurits"], [])
                ,(Atom "mother" [Const "margriet", Const "bernhard_jr"], [])
                ,(Atom "mother" [Const "margriet", Const "pieter_christiaan"], [])
                ,(Atom "mother" [Const "margriet", Const "floris"], [])
                ,(Atom "mother" [Const "beatrix", Const "alexander"], [])
                ,(Atom "mother" [Const "beatrix", Const "friso"], [])
                ,(Atom "mother" [Const "beatrix", Const "constantijn"], [])
                ,(Atom "mother" [Const "maxima", Const "amalia"], [])
                ,(Atom "mother" [Const "maxima", Const "alexia"], [])
                ,(Atom "mother" [Const "maxima", Const "ariane"], [])

                ,(Atom "husband" [Const "bernhard", Const "juliana"], [])
                ,(Atom "husband" [Const "claus", Const "beatrix"], [])
                ,(Atom "husband" [Const "pieter", Const "margriet"], [])
                ,(Atom "husband" [Const "alexander", Const "maxima"], [])
                ,(Atom "husband" [Const "friso", Const "mabel"], [])
                ,(Atom "husband" [Const "constantijn", Const "laurentien"], [])

                ,(Atom "female" [Const "irene"], [])
                ,(Atom "female" [Const "christina"], [])
                ,(Atom "female" [Const "amalia"], [])
                ,(Atom "female" [Const "alexia"], [])
                ,(Atom "female" [Const "ariane"], [])
                ,(Atom "female" [Var "X"], [Atom "mother" [Var "X", Var "_"]])
                ,(Atom "female" [Var "X"], [Atom "husband" [Var "_", Var "X"]])

                ,(Atom "male" [Const "maurits"], [])
                ,(Atom "male" [Const "bernhard_jr"], [])
                ,(Atom "male" [Const "pieter_christiaan"], [])
                ,(Atom "male" [Const "floris"], [])
                ,(Atom "male" [Var "X"], [Atom "husband" [Var "X", Var "_"]])
                ]
            
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
                    
program2 :: Program
program2 = map parseClause statements

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
                
falseFamilyQueries = [ [parse "female(alexander)"]
                     , [parse "mother(maxima, maurits)"]
                     , [parse "mother(X, laurentien)"]
                     , [parse "female(argOne, argTwo)"]
                     , [parse "mother(argOne)"]
                     ]

familyTest :: Bool
familyTest = (and $ map (fst) $ map (evalMulti familyProgram) familyQueries)            -- All statements are true
             &&
             (not $ or $ map (fst) $ map (evalMulti familyProgram) falseFamilyQueries)  -- All statements are false