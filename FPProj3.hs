module FPProj3 where

import Data.List
import Data.Maybe


-- =========== Types ===========

type Predicate = String

data Term 	= Var String
			| Const String
			deriving (Show, Eq)

data Atom	= Atom Predicate [Term] deriving (Show, Eq)

type Clause	= (Atom, [Atom])
type Query	= [Atom]

type Program	= [Clause]

type Substitution 	= [(Term, Term)]

type Solution		= (Bool, [Substitution])

-- =========== Unify ===========

unify :: Atom -> Atom -> Substitution
unify a1 a2 	= unify' [] a1 a2

unify' :: Substitution -> Atom -> Atom -> Substitution
unify' subs 	(Atom p1 (t1@(Var x):terms1))	(Atom p2 (t2:terms2))			| p1 == p2				= unify' ((t1, t2):subs) (Atom p1 terms1) (Atom p2 terms2)
unify' subs 	(Atom p1 ((Const x):terms1))	(Atom p2 ((Const y):terms2))	| p1 == p2 && x == y 	= unify' subs (Atom p1 terms1)	(Atom p2 terms2)
unify' _ 		_ 					_ 				 							= []

-- =========== Evaluate ===========

