{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module FPProj2 where

import Data.List
import Data.Maybe

import Debug.Trace

-- ========== Types etc. ==========

data Term	= Const String
			| Var String
			deriving (Show, Eq)

type Pred 	= String
data Atom 	= Atom Pred Term
			deriving (Show, Eq)

type Query	= [Atom]

type Clause	= (Atom, [Atom])

type Program	= [Clause]

type Substitution 	= (Term, Term)

-- ========== Helper functions ==========

isConst :: Term -> Bool
isConst (Const _)	= True
isConst _			= False

isVar :: Term -> Bool
isVar (Var _)		= True
isVar _				= False

predicate :: Atom -> Pred
predicate (Atom p _)	= p

term :: Atom -> Term
term (Atom _ t)	= t

-- ========== Expression type class ==========

class Expr e where
	(<=~) :: e -> Substitution -> e
	value :: e -> String

instance Expr Term where
	(Var x)		<=~ (Var y, r)		| x == y 	= r
	t	 		<=~	_				= t

	value (Const x)					= x
	value (Var x)					= x

instance Expr Atom where
	(Atom p t)	<=~ s	= Atom p (t <=~ s)

	value (Atom _ t)	= value t

-- ========== Rename ==========

rename :: Clause -> Query -> Clause
rename a@(Atom p t1, _) qs 	= rename' a sub
	where
		s = head $ dropWhile (`elem` (map value qs)) generateStrings
		sub = (t1, Var s)

rename' :: Clause -> Substitution -> Clause
rename' (a, as)	s 	= (a <=~ s, map (<=~ s) as)


generateStrings :: [String]
generateStrings = concat $ iterate (zipWith (++) aTOz) aTOz
	where aTOz = map (:[]) ['A'..'Z']

-- ========== Unify ==========

unify :: Atom -> Atom -> Maybe Substitution
unify (Atom p1 t1@(Var x))	(Atom p2 t2)	| p1 == p2	= Just (t1, t2)
unify a 					_ 				= Nothing

-- ========== Evaluate ==========

type Solution = (Bool, [Substitution])

evalOne :: Program -> Query -> Solution
evalOne []		_						= (False, [])
evalOne _		[]						= (True, [])
evalOne prog	(q@(Atom p term):qs)	= (solvable, substitutions)
	where
		s = findSubstitutions prog q
		(solvable', subs')	= evalOne prog qs

		solvable = case term of
			Var _ 	-> 	(s /= []) && solvable'
			Const _	-> q `elem` (map fst prog)

		substitutions = if qs == [] then s else intersect s subs'


findSubstitutions :: Program -> Atom -> [Substitution]
findSubstitutions []										_						= []
findSubstitutions _											(Atom _ (Const _))		= []
findSubstitutions p@(clause@(a@(Atom cpred cterm), as):cs)	q@(Atom qpred (Var s))	
	| cpred == qpred	= subs ({--trace ("\nsubmaybe = " ++ show subMaybe ++ "\n")--} subMaybe)
	| otherwise			= findSubstitutions cs q
	where
		subMaybe	= unify q a
		subs subM	= case subM of
			
			Just sub@(_, term) 	-> case term of
									Const _ -> sub : findSubstitutions cs q
									Var _ 	-> snd (trace ("rec = evalOne " ++ show p ++ " " ++ show as ++ " = " ++ show (evalOne p as)) (evalOne p as))

			Nothing 			-> findSubstitutions cs q


program :: Program
program 	= 	[(Atom "p" (Const "a"), []) 										--p(a).
				,(Atom "p" (Const "b"), [])											--p(b).
				,(Atom "p" (Const "c"), [])											--p(c).
				,(Atom "q" (Const "a"), [])											--q(a).
				,(Atom "q" (Const "b"), [])											--q(b).
				,(Atom "r" (Var "X"), [Atom "p" (Var "X")])							--r(X).
				--,(Atom "r" (Var "X"), [Atom "p" (Var "X"), Atom "q" (Var "X")])		--r(X) :− p(X),q(X).
				]