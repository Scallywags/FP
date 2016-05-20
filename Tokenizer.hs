module Tokenizer where

import Data.Char

import FPPrac.Trees
import FP_TypesEtc
import FP_ParserGen
import Lab6

-- ====================== STATES =========================

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
fsaOpSucess s = s /= S && s /= Error

testFsaOp :: [Char] -> Bool
testFsaOp cs = testFsa fsaOp fsaOpSucess cs

fsaBrack :: State -> Char -> State
fsaBrack S '(' 	= Q0
fsaBrack S ')' 	= Q0
fsaBrack _ _ 	= Error

fsaBrackSuccess :: State -> Bool
fsaBrackSuccess s = s == Q0

fsaBrace :: State -> Char -> State
fsaBrace S '{' 	= Q0
fsaBrace S '}' 	= Q0
fsaBrace _ _ 	= Error

fsaBraceSuccess :: State -> Bool
fsaBraceSuccess s = s == Q0

fsaSemi :: State -> Char -> State
fsaSemi S ';' 	= Q0
fsaSemi _ _		= Error

fsaSemiSuccess :: State -> Bool
fsaSemiSuccess s = s == Q0

testFsaBrack :: [Char] -> Bool
testFsaBrack cs = testFsa fsaBrack fsaBrackSuccess cs

fsaSpace :: State -> Char -> State
fsaSpace S ' '	= S
fsaSpace S '\t' = S
fsaSpace S '\n' = S
fsaSpace S '\r' = S
fsaSpace _ _ 	= Error

fsaSpaceSuccess :: State -> Bool
fsaSpaceSuccess S = True

testFsaSpace :: [Char] -> Bool
testFsaSpace cs = testFsa fsaSpace fsaSpaceSuccess cs

-- ======================================================

-- ==================== FIND TOKEN ======================

type FSA = (State -> Char -> State, State -> Bool)

findToken :: String -> State  -> String -> FSA -> Maybe (String, String)
findToken _ Error _ _ 							= Nothing
findToken "" state soFar (ffsa, success)		| success state 	= Just (soFar, "")
												| otherwise			= Nothing
findToken (x:xs) state soFar (ffsa, success) 	| success state && maybeNext == Nothing	= Just (soFar, (x:xs))
												| otherwise								= maybeNext
	where
		maybeNext = findToken xs (ffsa state x) (soFar ++ [x]) (ffsa, success)


scan :: String -> [String]
scan "" 	= []
scan (c:cs)	= case foundTokenMaybe of
						Nothing -> scan cs
						Just (tokenVal, rest) -> tokenVal:scan rest
	where
		fsa | isDigit c || c == '~'	|| c == '.'	= (fsaNumb, fsaNumbSuccess)
			| c `elem` " \n\r\t"				= (fsaSpace, fsaSpaceSuccess)
			| isLetter c 						= (fsaIdf, fsaIdfSuccess)
			| c == '(' || c == ')'				= (fsaBrack, fsaBrackSuccess)
			| c `elem` "-+*/<>=&|"				= (fsaOp, fsaOpSucess)
			| c == ';'							= (fsaSemi, fsaSemiSuccess)
			| c == '{' || c == '}'				= (fsaBrace, fsaBraceSuccess)
			| otherwise							= error ("Found unparsable character: " ++ [c])
		foundTokenMaybe = findToken (c:cs) S "" fsa


tokenize :: String -> [Token]
tokenize s = zip3 types values [0..]
				where (types, values)	= unzip (filter (\(t, v) -> t /= FP_TypesEtc.Space) (map toToken (scan s)))

toToken :: String -> (Alphabet, String)
toToken t@"true"	= (Boolean, t)
toToken f@"false"	= (Boolean, f)
toToken "("			= (Bracket, "(")
toToken ")"			= (Bracket, ")")
toToken "{"			= (Brace, "{")
toToken "}"			= (Brace, "}")
toToken ";"			= (Semi, ";")

toToken (c:cs)		| c `elem` "-+*/<>=&|"								= (Op, (c:cs))
toToken cs 			| isKeyWord cs 										= (ResWord, cs)
toToken (c:cs)		| c == ' ' || c == '\t' || c == '\r' || c == '\n'	= (FP_TypesEtc.Space, (c:cs))
toToken (c:cs)		| isLetter c 										= (Var, (c:cs))
toToken (c:cs)		| isDigit c || c == '~' || c == '.'					= (Nmbr, (c:cs))


toToken	_			= error "Irrecognizable string!"


testPrint = prpr (parse myGrammar Expr (tokenize "a+b-4"))