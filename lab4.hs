module Lab4 where

import Data.Char
import FPPrac.Trees

--1

data BinTree a b 	= BinNode a (BinTree a b) (BinTree a b)
					| BinLeaf b
						deriving (Show, Eq)

data Unit = Unit

type Tree1a = BinTree Int Int
type Tree1b = BinTree Unit (Int, Int)
type Tree1c = BinTree Int Unit

pp :: (Show a, Show b) => BinTree a b -> RoseTree
pp (BinNode x r l)	= RoseNode (show x) [pp r, pp l]
pp (BinLeaf y)		= RoseNode (show y) []


--2

type Expr	= BinTree Op Int
data Op		= Plus | Minus | Mult | Div | Pow deriving (Show, Eq)
data NT 	= E | O | N | L deriving (Show, Eq)

parse :: NT -> [Char] -> (Expr, [Char])
parse E ('(':r0)		= (BinNode op e0 e1 , r3)
						where 
							(e0, r1) 				= parse E r0
							(BinNode op _ _, r2)	= parse O r1
							(e1, ')':r3)			= parse E r2

parse E s@(c:r0)		| isDigit c 				= (leaf, r1)
						| otherwise					= error ("Expected digit but got " ++ [c])
							where (leaf, r1) = parse N s

parse O (c:r0)			= (BinNode op undefined undefined, r0)
						where
							op = case c of
								'+'	-> Plus
								'-'	-> Minus
								'*'	-> Mult
								'/'	-> Div
								'^'	-> Pow
								_	-> error ("Invalid operand: " ++ [c])

parse N (c:r0)			= (BinLeaf (read [c]), r0)

type Expr2b	= BinTree Op (Either Int Char)

parse2b :: NT -> [Char] -> (Expr2b, [Char])
parse2b E ('(':r0)		= (BinNode op e0 e1 , r3)
						where 
							(e0, r1) 				= parse2b E r0
							(BinNode op _ _, r2)	= parse2b O r1
							(e1, ')':r3)			= parse2b E r2

parse2b E s@(c:r0)		| isDigit c 				= (leftNumber, r1n)
						| isLetter c 				= (rightLetter, r1l)
						| otherwise					= error ("Expected either digit or letter but got " ++ [c])
							where
								(leftNumber, r1n) = parse2b N s
								(rightLetter, r1l) = parse2b L s

parse2b O (c:r0)			= (BinNode op undefined undefined, r0)
						where
							op = case c of
								'+'	-> Plus
								'-'	-> Minus
								'*'	-> Mult
								'/'	-> Div
								'^'	-> Pow
								_	-> error ("Invalid operand: " ++ [c])

parse2b N (c:r0)			= (BinLeaf (Left (read [c])), r0)

parse2b L (c:r0)			= (BinLeaf (Right c), r0)


--3

data State = S | Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14 | Q15 | Q16 | Q17 | Q18 | Q19 deriving (Show, Eq)

testFsa :: (State -> Char -> State) -> (State -> Bool) -> String -> Bool
testFsa fsa success cs = success (foldl fsa S cs)

fsaNumb :: State -> Char -> State
fsaNumb S c		| c == '~'		= Q0
				| c == '.'		= Q2
				| isDigit c 	= Q1
fsaNumb Q0 c 	| isDigit c 	= Q1
				| c == '.'		= Q2
fsaNumb Q1 c 	| isDigit c 	= Q1
				| c == '.'		= Q2
fsaNumb Q2 c 	| isDigit c 	= Q3
fsaNumb Q3 c 	| isDigit c 	= Q3

fsaNumbSuccess :: State -> Bool
fsaNumbSuccess s 	= s == Q3 || s == Q1

testFsaNumb :: [Char] -> Bool
testFsaNumb cs = testFsa fsaNumb fsaNumbSuccess cs

fsaIdf :: State -> Char -> State
fsaIdf S c 	| isLetter c 	= Q0
fsaIdf Q0 c | isLetter c 	= Q0
			| isDigit c 	= Q0

fsaIdfSuccess :: State -> Bool
fsaIdfSuccess s = s == Q0

testFsaIdf :: [Char] -> Bool
testFsaIdf cs = testFsa fsaIdf fsaIdfSuccess cs

fsaOp :: State -> Char -> State
fsaOp S '-'	= Q0
fsaOp S '+' = Q2
fsaOp S '^' = Q4
fsaOp S '*' = Q5
fsaOp S '/' = Q6
fsaOp S '=' = Q7
fsaOp S '<' = Q11
fsaOp S '>' = Q9
fsaOp S '&' = Q13
fsaOp S '|' = Q14

fsaOp Q0 '-' = Q1
fsaOp Q2 '+' = Q3
fsaOp Q6 '=' = Q17
fsaOp Q7 '=' = Q8
fsaOp Q9 '=' = Q10
fsaOp Q11 '=' = Q12
fsaOp Q13 '&' = Q15
fsaOp Q14 '|' = Q16

fsaOpSucess :: State -> Bool
fsaOpSucess s = s /= S && s /= Q13 && s /= Q14 && s /= Q7

testFsaOp :: [Char] -> Bool
testFsaOp cs = testFsa fsaOp fsaOpSucess cs

fsaBrack :: State -> Char -> State
fsaBrack S '(' = Q0
fsaBrack S ')' = Q0

fsaBrackSuccess :: State -> Bool
fsaBrackSuccess s = s == Q0

testFsaBrack :: [Char] -> Bool
testFsaBrack cs = testFsa fsaBrack fsaBrackSuccess cs

fsaSpace :: State -> Char -> State
fsaSpace S ' '	= S

fsaSpaceSuccess :: State -> Bool
fsaSpaceSuccess S = True

testFsaSpace :: [Char] -> Bool
testFsaSpace cs = testFsa fsaSpace fsaSpaceSuccess cs


{-
type Token = String

tokenize :: String -> [Token]
tokenize [] 	= []
tokenize (c:cs)	= 
-}