module FPProj2 where

import Data.List
import Data.Maybe
import Data.Char

{- ===============================================================

                            README
                            
         To evaluate an expression you are given two options:
         
         1. Call evalOne (yourProgram) (yourQuery)
            Example:    evalOne program [Atom "p", (Var "X")]
            
         2. Call parse (yourQueryAsAStringInPrologNotation)
            Example:    parse "r(X)"

================================================================-}



-- ========== Types etc. ==========

data Term	= Const String
			| Var String
			deriving (Show, Eq)

type Pred 	= String
data Atom 	= Atom Pred Term
			deriving (Show, Eq)

type Query	        = [Atom]

type Clause	        = (Atom, [Atom])

type Program	    = [Clause]

type Substitution 	= (Term, Term)

-- The solution we return consists of a boolean and a list of possible substitutions
-- The boolean tells the user whether the query is true or not.
type Solution       = (Bool, [Substitution])


{- We want to be able to order atoms so that when we evaluate a query,
   we first check the atoms containing a constant.
   We do this because checking a constant on average takes less time than
   checking a variable because then we need to deal with recursion.
   When checking a query that would evaluate to false we now check it more
   efficiently because we start checking the constants.
-}
instance Ord Atom where
	compare (Atom _ (Var _)) (Atom _ (Var _))		= EQ 
	compare (Atom _ (Const _)) (Atom _ (Const _))	= EQ
	compare (Atom _ (Var _)) (Atom _ (Const _)) 	= GT 
	compare (Atom _ (Const _)) (Atom _ (Var _)) 	= LT 




-- ========== Helper functions ==========

isConst :: Term -> Bool
isConst (Const _)	    = True
isConst _			    = False

isVar :: Term -> Bool
isVar (Var _)		    = True
isVar _				    = False

predicate :: Atom -> Pred
predicate (Atom p _)    = p

term :: Atom -> Term
term (Atom _ t)	        = t



-- ========== Expression type class ==========

-- We defined "<=~" as the operator used for substituting
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

-- Unify return a substitution of the first atom to the second atom if it finds any.
-- When the first atom is a constant it can not find any substitutions because we should not substitute constants.
unify :: Atom -> Atom -> Maybe Substitution
unify (Atom p1 t1@(Var x))	(Atom p2 t2)	| p1 == p2	= Just (t1, t2)
unify _ 					_ 				= Nothing

-- ========== Evaluate ==========

-- The function the user should be interested in.
-- EvalOne generates solutions for a query given a program (or it doesn't if the query cannot be proven).
evalOne :: Program -> Query -> Solution
evalOne []		_						= (False, [])
evalOne _		[]						= (True, [])
evalOne prog	query 					= (solvable, substitutions)
	where
        -- We sort the atoms in the query. See above at Types etc. for an explanation why.
		(q@(Atom p term):qs) = sort query
		s = findSubstitutions prog prog q
		(solvable', subs')	= evalOne prog qs

		solvable = case term of
            -- Solvable if substitutions can be found and the rest of the query is solvable.
			Var _ 	-> 	(s /= []) && solvable'
            
            -- Solvable if the constant is the lefthandside of a clause in the program and the rest of the query is solvable
            -- or if there exists a substitution where the lefthandside of a clause in the program can be replaced by this constant.
			Const _	-> case q `elem` (map fst prog) of
								True 	-> 	if qs == [] then True else solvable'
								False 	->	(Var "X", term) `elem` subsrec
									where
										varAtom = Atom p (Var "X")
										(_, subsrec)	= evalOne prog [varAtom]
                        
                        -- When term is a variable we take the intersection of it's substitutions with substitutions found for the rest of the query.
		substitutions 	| isVar term 	=	if qs == [] then s else intersect s subs'
                        
                        -- If not solvable, we return no substitutions.
						| not solvable	= 	[]
                        
                        -- Otherwise (Constant and solvable) we return the substitutions of the rest of the query.
						| otherwise		= 	subs'


-- FindSubstitutions gets to programs. The first program is the whole program,
-- the second program is a reduced program
findSubstitutions :: Program -> Program -> Atom -> [Substitution]
findSubstitutions _		[]										_						= []
findSubstitutions _		_										(Atom _ (Const _))		= []
findSubstitutions p 	((a@(Atom cpred cterm), as):cs)	q@(Atom qpred (Var s))	
	| cpred == qpred	= subs subMaybe
    -- If this clause is not applicable (different predicates) try again with the reduced program.
	| otherwise			= findSubstitutions p cs q
	where
        -- Here we try to find a substitution for q so that it matches a.
		subMaybe	= unify q a
        
		subs subM	= case subM of
			Just sub@(_, term) 	-> case term of
                                    -- If the substitution found replaces a variable with
                                    -- a constant the substitution should be returned along with the other
                                    -- substitutions we might find.
									Const _ 	-> sub : findSubstitutions p cs q
                                    
                                    -- If the substitution found replaces a variable with another variable
                                    -- we try to find the substitutions for that variable.
									Var _ 		-> snd (evalOne p as)
                                    
            -- If no substitution is found by unify we try again with the reduced program.
			Nothing 			-> findSubstitutions p cs q


-- ========== Test program ==========

-- An extended version of the example program
program :: Program
program 	= 	[(Atom "p" (Const "a"), []) 										--p(a).
				,(Atom "p" (Const "b"), [])											--p(b).
				,(Atom "p" (Const "c"), [])											--p(c).
				,(Atom "q" (Const "a"), [])											--q(a).
				,(Atom "q" (Const "b"), [])											--q(b).
                ,(Atom "r" (Var "X"), [Atom "q" (Var "X")])                         --r(X) :- q(X).
				,(Atom "s" (Var "X"), [Atom "p" (Var "X"), Atom "q" (Var "X")])		--s(X) :− p(X),q(X).
                ,(Atom "t" (Var "X"), [Atom "p" (Var "X"), Atom "q" (Const "b")])   --t(X) :- p(X),q(b).
                ,(Atom "u" (Var "X"), [Atom "p" (Const "c"), Atom "r" (Var "X")])   --u(X) :- p(c),r(X).
                ,(Atom "v" (Const "a"), [Atom "q" (Var "X"), Atom "p" (Const "c"), Atom "r" (Var "X"), Atom "s" (Var "X"), Atom "q" (Const "b")]) 	--v(a) :- q(X),p(c),r(X),s(X),q(b).
                ,(Atom "w" (Var "Y"), [Atom "r" (Var "Y")])							--w(Y) :- r(Y).
				]

-- ========== Test ==========

queries :: [Query]
queries = [ [Atom "p" (Const  "a")]   --p(a)?
          , [Atom "p" (Const  "b")]   --p(b)?
          , [Atom "p" (Const  "c")]   --p(c)?
          , [Atom "q" (Var    "X")]   --q(X)?
          , [Atom "r" (Const  "a")]   --r(a)?
          , [Atom "r" (Var    "X")]   --r(X)?
          , [Atom "r" (Var    "A")]   --r(A)?
          , [Atom "s" (Var    "X")]   --s(X)?
          , [Atom "s" (Const  "a")]   --s(a)?
          , [Atom "t" (Var    "X")]   --t(X)?
          , [Atom "t" (Const  "a")]   --t(a)?
          , [Atom "u" (Const  "a")]   --u(a)?
          , [Atom "u" (Var    "X")]   --u(X)?
          , [Atom "v" (Var    "X")]   --v(X)?
          , [Atom "v" (Var    "Y")]   --v(Y)?
          , [Atom "v" (Const  "a")]   --v(a)?
          ]

falseQueries :: [Query]
falseQueries = [ [Atom "p" (Const "d")]
               , [Atom "a" (Const "a")]
               , [Atom "r" (Const "c")]
               , [Atom "s" (Const "c")]
               , [Atom "t" (Const "d")]
               , [Atom "u" (Const "c")]
               , [Atom "v" (Const "b")]
               , [Atom "w" (Const "c")]
               , [Atom "p" (Const "a"), Atom "p" (Const "d"), Atom "p" (Var "X")] -- This one fails !
               , [Atom "p" (Var   "X"), Atom "q" (Const "c")]
               , [Atom "p" (Const "d"), Atom "p" (Var   "X")]
               , [Atom "p" (Var   "X"), Atom "a" (Var   "X")]
               ]

test :: Bool
test = (and $ map fst $ map (evalOne program) queries) &&
       (not $ or $ map fst $ map (evalOne program) falseQueries)
       
       
-- ========== Parser ==========
{- We made a parser that recognizes atoms like they are defined in prolog. (But only one character for predicates and constants and variables!)
   This method immediately calls the evalOne function so you only have to type something like:
        Example: parse "r(X)". The result is "(True,[(Var "X", Const "a"), (Var "X", Const "b")])"
        Example: parse "p(a)". The result is "(True, [])"
   NB:  This parser only recognizes queries which consist of a single atom.
-}
parse :: String -> Solution
parse (p:'(':c:')':[])  | isLower c = evalOne program [Atom [p] (Const [c])]
                        | otherwise = evalOne program [Atom [p] (Var [c])]
parse _                 = error "Parse error"