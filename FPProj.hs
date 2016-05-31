{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Proj where

import Data.List

-- #1

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

-- #2

type Predicate 	= String

data Var		= Var String deriving (Show, Eq)
data Const 		= Const String deriving (Show, Eq)

data Atom2	= ConstAtom (Predicate, Const)
			| VarAtom (Predicate, Var)
			deriving (Show, Eq)

type Clause2 = (Atom2, [Atom2])

type Program2 = [Clause2]

type Query2 = Atom2

class Expr e where
	(<=~) :: e -> (String, Either Const Var) -> Either e Const

instance Expr Atom2 where
	(VarAtom (p, Var v)) 	<=~ (x, Right (Var a))	| x == v 	= Left (VarAtom (p, Var a))
	(VarAtom (p, Var v)) 	<=~ (x, Left (Const a))	| x == v 	= Left (ConstAtom (p, Const a))
 	e 						<=~ _					= Left e

instance Expr Const where
	c <=~ _	= Left c

instance Expr Var where
	(Var s)	<=~	(x, Right (Var a))	| x == s	= Left (Var a)
	(Var s)	<=~ (x, Left (Const a))	| x == s 	= Right (Const a)
	e 		<=~ _					= Left e

