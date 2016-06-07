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

unify :: Atom -> Atom -> Maybe Substitution
unify a1 a2     = unify' [] a1 a2

unify' :: Substitution -> Atom -> Atom -> Maybe Substitution
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
unify' subs     (Atom p1 [])                    (Atom p2 [])                    | p1 == p2              = Just subs
unify' _        _                               _                               = Nothing

-- =========== Expression type class ===========

class Expr e where
    (<=~) :: e -> (Term, Term) -> e

    rename :: e -> String -> e

instance Expr Term where
    (Var x)         <=~     (Var y, replacement)    | x == y    = replacement
                                                    | otherwise = Var x
    (Var x)         <=~     (Const y, _)            = Var x
    (Const x)       <=~     _                       = Const x

    rename (Var _)      name                        =  Var name
    rename (Const x)    _                           =  Const x

instance Expr Atom where
    (Atom p ts)     <=~     sub     = Atom p (map (<=~ sub) ts)

    rename (Atom p terms)   name    = Atom p (map (`rename` name) terms)

instance Expr Clause where
    (a, as)         <=~     sub     = (a <=~ sub, map (<=~ sub) as)

    rename (a, as)          name    = (rename a name, (map (`rename` name) as))

instance Expr Program where
    prog            <=~     sub     = map (<=~ sub) prog

    rename prog             name   = map (`rename` name) prog

-- =========== Evaluate ===========

evalMulti :: Program -> Query -> Solution
evalMulti prog  []      = true
--evalMulti prog (q:qs)   = 
--    where


true :: Solution
true = (True, [])

false :: Solution
false = (False, [])

findSubs :: Program -> Atom -> Query -> Solution
findSubs prog _ []  = true
findSubs prog (Atom cpred (t@(Var x):ts)) (q@(Atom qpred qterms):qs)    = false --TODO


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