module Proj1 where

import Data.List

data Atom 		= A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D deriving (Show, Eq)

type Clause 	= (Atom, [Atom])

type Program	= [Clause]

type Query		= [Atom]

programme :: Program
programme	= 	[(A0, [])
				,(A1, [])
				,(A2, [])
				,(B0, [A0, A1])
				,(B1, [A1, A2])
				,(B2, [A1, A2, D])
				,(C0, [B0, B1])
				,(C1, [B0, B1, B2])
				]

evalProp :: Program -> Query -> Bool
evalProp [] _			= False
evalProp _ [] 			= True
evalProp p qs 			| not $ all (\q -> q `elem` (map fst matches)) qs = False
						| otherwise = and [or [evalProp p as | (a, as) <- inner] | inner <- grouped]
	where
		matches = [(a, as) | (a, as) <- p, q <- qs, q == a]
		grouped = groupBy (\(a, as) (b, bs) -> a == b) matches