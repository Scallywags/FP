module Proj1 where

import Data.List


{- ===============================================================

                            README
                            
 To evaluate an expression call evalProp (yourProgram) (yourQuery)
                Example: evalProp program [C1]

================================================================-}



-- ============ Types ============

data Atom 		= A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D deriving (Show, Eq)

type Clause 	= (Atom, [Atom])

type Program	= [Clause]

type Query		= [Atom]


-- The example program mentioned in the project documents
{-
    a0.                     - Should be true
    a1.                     - Should be true
    a2.                     - Should be true
    
    b0 :- a0, a1.           - Should be true
    b1 :- a1, a2.           - Should be true
    b2 :- a1, a2, d.        - Should be false (closed world assumption)
    
    c0 :- b0, b1.           - Should be true
    c1 :- b0, b1, b2.       - Should be false
-}
program :: Program
program	= 	[(A0, [])
				,(A1, [])
				,(A2, [])
				,(B0, [A0, A1])
				,(B1, [A1, A2])
				,(B2, [A1, A2, D])
				,(C0, [B0, B1])
				,(C1, [B0, B1, B2])
				]


-- This is the function that, when given a program and a query, can check if the propositions in the query are true.
evalProp :: Program -> Query -> Bool

-- If the program has no clauses we assume that our query is false since we live under the closed world assumption.
evalProp [] _			= False

-- We consider an empty query to be true.
evalProp _ [] 			= True

-- We check if every atom in our query has a clause in matches. If it hasn't, the query is false (closed world assumption).
evalProp p qs 			| not $ all (\q -> q `elem` (map fst matches)) qs = False

-- For every atom in the query we check if there is a clause in the program that by means of recursion eventually evaluates to true.
-- This can be only one clause per atom, but all atoms in the query should have a match.
						| otherwise = and [or [evalProp p as | (a, as) <- inner] | inner <- grouped]
	where
    
        -- Matches is a list of all clauses in the given program that could be unified with an atom in the query
		matches = [(a, as) | (a, as) <- p, q <- qs, q == a]
        
        -- Grouped groups the matches by atom in the query.
        -- It returns a list consisting of lists.
        -- The list has a list for every atom in the query and every
        -- element in a list is a clause in the program that has a lefthandside equal to the atom of that list.
		grouped = groupBy (\(a, as) (b, bs) -> a == b) matches


-- ========== Test ==========

-- Queries that should be true
queries :: [Query]
queries = [ [A0]
          , [A1]
          , [A2]
          , [B0]
          , [B1]
          , [C0]
          , [A0, A1, B0]
          , [C0, B0, A2, C0, B0]
          ]

-- Queries that should be false
falseQueries :: [Query]
falseQueries = [ [D]
               , [B2]
               , [C1]
               , [A0, D]
               , [A1, B0, C0, C1]
               , [C1, D]
               ]

-- The test itself. (This test tests program as defined above)
test :: Bool
test = (and $ map (evalProp program) queries) &&
       (not $ or $ map (evalProp program) falseQueries)