{-# LANGUAGE RecordWildCards #-}

import Data.Char
import Data.Maybe


data Test	= A Int
			| B Bool
			| C Char


f :: Test -> Int
f (A a)		= a
f (B b)		= if b then 1 else 0
f (C c)		= ord c

--same but with 'case _ of' -expression

g x = case x of
		A a -> a
		B b -> if b then 1 else 0
		C c -> ord c

{-

Maybe a 	= Nothing
			| Just a

a can be any type!




-}

division :: (Fractional a, Eq a) => a -> a -> Maybe a
division x 0	= Nothing
division x y	= Just (x/y)

{-

maybe 10 (+1) (division 2 5)

handy functions: catMaybes, fromJust, maybe, cat

-}



{-

Also handy: Either!

Either a b 		= Left a
				| right b

-}



{-

Records! :D

syntatic sugar for algebraic datatypes

data Person = Person	{ firstName :: String
						, famName :: String
						, birthDay :: (Int, Int, Int)
						}

p = Person {firstName = "John", famName = "Johnson", birthDay = (7, 6, 1995)}

selection: firstName p 				--also: map firstName ps


records can be partial:

q = Person { firstName = "Bill" } --works as long as you dont call the other record functions

updating records:

p' = p {firstName = "Dick"}

Pattern matching:

f (Person {firstName = "John"})		= ...
f (Person {firstName = fnm})		= ... fnm ...
f (r @ Person {firstName = fnm}) 	= ... fnm ... r ...


data Person 	= Person { name :: String, age :: Int }
				| Company { name :: String, address :: String }

name is a function that gets a Person and returns a String!

also: ... where Rec {x=x0, y=y0}	= ... x0 ... y0 ...
even: Rec {x=x, y=y}				= ... x ... y ....

function definition:
f (Rec {x = x})		= Rec {x = x + 1}
when calling:
f (Rec {x = 3})		=> Rec {x = 4}
THIS IS NOT ASSIGNMENT!

pattern {x=x, y=y, ...}
-> equivalent: Rec {..} --ONLY WORKSWHEN THE COMPILER DIRECTIVE -# LANGUAGE RecorWildCars #- is specified


-}



{-

Regular Expressions and Grammars

Q -> a Q
Q -> b R
R -> a R
R -> b Q
R -> epsilon

Terminals: {a, b}
NonTerminals: {Q, R}

Start = Q
Final = R


How to model this dfa?

data DFA_QR_STATE = Q | R

--use a function for transitions! arguments are the state and the characaters you are reading

fsa0 :: DFA_QR_STATE -> [Char] -> DFA_QR_STATE
fsa0 s ""		= s
fsa0 Q ('a':xs)	= fsa0 Q xs
fsa0 Q ('b':xs) = fsa0 R xs
fsa0 R ('a':xs) = fsa0 R xs
fsa0 R ('b':xs) = fsa0 Q xs

--Can be done with just a state and one character! :D

fsa1 :: DFA_QR_STATE -> Char -> DFA_QR_STATE

fsa1 Q 'a' = Q
fsa1 Q 'b' = R
fsa1 R 'a' = R
fsa1 R 'b' = Q

testFSA1 = foldl fsa1 Q "aabaabaaaabaa"	--foldl takes care of the base case (start state)

--even more pretty:

fsa2 :: DFA_QR_STATE -> Char -> DFA_QR_STATE
fsa2 = \s x -> case s of
						Q 	| x == 'a' -> Q
							| x == 'b' -> R
						R 	| x == 'a' -> R
							| x == 'b' -> Q		

--allows for abstraction, not just equality of s and x.
--and testing can still be done with a fold.

-}

{-

Parsing Mathematical Expressions :D

E -> N
E -> '(' E O E ')'
N -> '0' | '1' | '2' | ...
O -> '+' | '-' | '*' | ...

Non-terminals 	: E, N, O
Terminals: 		: 0, 1, 2, ..., +, -, *, ..., '(', ')'
Start symbol	: E


Parsing:: transforms a list of tokens (string) into a tree
parse :: Alphabet -> [Token] -> (Tree, [Token])

parse S tokens 	= ...
				where
					...
					
X -> S T
S -> A B C
A -> ....

parse :: NT -> String -> (Tree, String)
parse S tokens = NodeS ta tb tc, rc
			where
				(ta, ra)	= parse A tokens
				(tb, rb)	= parse B ra
				(tc, rc)	= parse C rb

--Pattern match for all rules, not just S but also X, T, A, B and C


-}