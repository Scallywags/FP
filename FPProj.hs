{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Proj where

import Data.Char
import Data.Either
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

data Atom2	= ConstAtom Predicate Const
			| VarAtom Predicate Var
			deriving (Show, Eq)

type Clause2 = (Atom2, [Atom2])

type Program2 = [Clause2]

type Query2 = Atom2

type Substitution = (String, Either Const Var)

class Expr e where
	(<=~) :: e -> Substitution -> Either e Const

instance Expr Atom2 where
	(VarAtom p (Var v)) 	<=~ (x, Right (Var a))	| x == v 	= Left (VarAtom p (Var a))
	(VarAtom p (Var v)) 	<=~ (x, Left (Const a))	| x == v 	= Left (ConstAtom p (Const a))
 	e 						<=~ _					= Left e

instance Expr Const where
	c <=~ _	= Right c

instance Expr Var where
	(Var s)	<=~	(x, Right (Var a))	| x == s	= Left (Var a)
	(Var s)	<=~ (x, Left (Const a))	| x == s 	= Right (Const a)
	e 		<=~ _					= Left e


rename :: Query2 -> Clause2 -> Clause2
rename q@(VarAtom qp (Var qv))	c@(VarAtom cp (Var cv), rhs)	| cp == qp && cv == qv	= (VarAtom cp (Var newName), newRhs)
																| otherwise				= c
	where
		newName	= getNewName qv (map atomValue rhs)
		newRhs	=  lefts $ map (<=~ (cv, Right (Var newName))) rhs

atomValue :: Atom2 -> String
atomValue (ConstAtom _ (Const c))	= c
atomValue (VarAtom _ (Var v))		= v

getNewName :: String -> [String] -> String
getNewName ""	_			= "This is totally not an easter egg"
getNewName (x:xs) excluded 	| result `elem` excluded	= getNewName result excluded
							| otherwise					= result
	where
		x'		= nextCapital x
		result	= (x':xs)

nextCapital :: Char -> Char
nextCapital x = chr $ (+65) $ (`mod` 26) $ (\x -> x-65) $ (+1) $ ord x

unify :: Atom2 -> Atom2 -> Maybe Substitution
unify (VarAtom p1 (Var v1))	(VarAtom p2 (Var v2))		| p1 == p2 	= Just (v1, Right (Var v2))
unify (VarAtom p1 (Var v1)) (ConstAtom p2 (Const v2))	| p1 == p2 	= Just (v1, Left (Const v2))
unify _						_							= Nothing

programme2 :: Program2
programme2 =	[ --TODO
				]