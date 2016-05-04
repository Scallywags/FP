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

data State = S | Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14 | Q15 | Q16 | Q17 | Q18 | Q19 | Error deriving (Show, Eq)

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
fsaNumb _ _		= Error

fsaNumbSuccess :: State -> Bool
fsaNumbSuccess s 	= s == Q3 || s == Q1

testFsaNumb :: [Char] -> Bool
testFsaNumb cs = testFsa fsaNumb fsaNumbSuccess cs

fsaIdf :: State -> Char -> State
fsaIdf S c 	| isLetter c 	= Q0
fsaIdf Q0 c | isLetter c 	= Q0
			| isDigit c 	= Q0
fsaIdf _ _	= Error

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
fsaOp S '.' = Q18

fsaOp Q0 '-' = Q1
fsaOp Q2 '+' = Q3
fsaOp Q6 '=' = Q17
fsaOp Q7 '=' = Q8
fsaOp Q9 '=' = Q10
fsaOp Q11 '=' = Q12
fsaOp Q13 '&' = Q15
fsaOp Q14 '|' = Q16
fsaOp _ _ 	= Error

fsaOpSucess :: State -> Bool
fsaOpSucess s = s /= S && s /= Q13 && s /= Q14 && s /= Q7 && s /= Error

testFsaOp :: [Char] -> Bool
testFsaOp cs = testFsa fsaOp fsaOpSucess cs

fsaBrack :: State -> Char -> State
fsaBrack S '(' 	= Q0
fsaBrack S ')' 	= Q0
fsaBrack _ _ 	= Error

fsaBrackSuccess :: State -> Bool
fsaBrackSuccess s = s == Q0

testFsaBrack :: [Char] -> Bool
testFsaBrack cs = testFsa fsaBrack fsaBrackSuccess cs

fsaSpace :: State -> Char -> State
fsaSpace S ' '	= S
fsaSpace _ _ 	= Error

fsaSpaceSuccess :: State -> Bool
fsaSpaceSuccess S = True

testFsaSpace :: [Char] -> Bool
testFsaSpace cs = testFsa fsaSpace fsaSpaceSuccess cs

data TokenType = Number | Identifier | Operator | BracketOpen | BracketClose | WhiteSpace deriving (Show, Eq)
type TokenValue = String
type Token = (TokenType, TokenValue)
type FSA = (State -> Char -> State, State -> Bool)

isOpChar :: Char -> Bool
isOpChar c = c `elem` "-+*/<>=&|."

findToken :: String -> State  -> TokenValue -> FSA -> Maybe (TokenValue, String)
findToken _ Error _ _ 							= Nothing
findToken "" state soFar (ffsa, success)		| success state 	= Just (soFar, "")
												| otherwise			= Nothing
findToken (x:xs) state soFar (ffsa, success) 	| success state && maybeNext == Nothing	= Just (soFar, (x:xs))
												| otherwise								= maybeNext
	where
		maybeNext = findToken xs (ffsa state x) (soFar ++ [x]) (ffsa, success)

tokenize :: String -> [Token]
tokenize "" 	= []
tokenize (c:cs)	= case foundTokenMaybe of
						Nothing -> tokenize cs
						Just (tokenVal, rest) -> (toToken tokenVal):tokenize rest
	where
		fsa | isDigit c || c == '~'		= (fsaNumb, fsaNumbSuccess)
			| c == ' '					= (fsaSpace, fsaSpaceSuccess)
			| isLetter c 				= (fsaIdf, fsaIdfSuccess)
			| c == '(' || c == ')'		= (fsaBrack, fsaBrackSuccess)
			| isOpChar c 				= (fsaOp, fsaOpSucess)
			| otherwise					= error ("Found unparsable character: " ++ [c])
		foundTokenMaybe = findToken (c:cs) S "" fsa

toToken :: TokenValue -> Token
toToken []			= (undefined, [])
toToken val@(x:xs)	| isDigit x || x == '~'		= (Number, val)
					| x == ' '					= (WhiteSpace, val)
					| isLetter x 				= (Identifier, val)
					| x == '('					= (BracketOpen, val)
					| x == ')'					= (BracketClose, val)
					| isOpChar x 				= (Operator, val)
					| otherwise 				= error ("Invalid token value: " ++ val)

--4

type ExprTree = BinTree Token Token

parse4 :: String -> ExprTree
parse4 input 	= tree
	where (tree, tokens) = parse4' (filter (\(tt, tv) -> tt /= WhiteSpace) (tokenize input))

parse4' :: [Token] -> (ExprTree, [Token])
parse4'	[] 														= (BinLeaf (WhiteSpace, "The End"), [])
parse4' (leaf@(Number, _):ts)									= (BinLeaf leaf, ts)
parse4' (leaf@(Identifier, _):ts)								= (BinLeaf leaf, ts)
parse4' ((BracketOpen, _):e1:op:e2:(BracketClose, _):ts)		= case ts of
																	[] 								-> (BinNode op (fst (parse4' [e1])) (fst (parse4' [e2])), ts)
																	(opToken@(Operator, opStr):r0) 	-> ((BinNode opToken subLeft subRight), rest)
																		where
																			(left, _) = parse4' [e1]
																			(right, _) = parse4' [e2]
																			(subRight, rest) = (parse4' r0)
																			subLeft = BinNode op left right
																					
--5

valueOfOp :: Token -> (Float -> Float -> Float)
valueOfOp (Operator, "*")	= (*)
valueOfOp (Operator, "/")	= (/)
valueOfOp (Operator, "+")	= (+)
valueOfOp (Operator, "-")	= (-)
valueOfOp (Operator, "^")	= (**)

valueOf :: TokenValue -> Float
valueOf "a"	= 5
valueOf "b"	= -2
valueOf "c"	= 100

valueOfFloat :: String -> Float
valueOfFloat ('~':xs)	= (-1) * (read xs)
valueOfFloat xs			= read xs

eval :: String -> Float
eval input = eval' (parse4 input)

eval' :: ExprTree -> Float
eval' (BinLeaf (Number, numString))	 	= valueOfFloat numString
eval' (BinLeaf (Identifier, idfString))	= valueOf idfString
eval' (BinNode op e1 e2)				= (valueOfOp op) (eval' e1) (eval' e2)
