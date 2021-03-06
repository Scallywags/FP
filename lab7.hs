module Lab7 where

import Data.List

data Expr	= Const Int
			| Var String
			| Boolean Bool
			| ITE Expr Expr Expr
			| BinOp String Expr Expr
			| App Expr Expr
			| TwoTup Expr Expr
			| ThreeTup Expr Expr Expr
			| Lambda Type Expr
			deriving (Show, Eq)

data Type 	= IntType
			| BoolType
			| FunType Type Type
			| TupType [Type]
			deriving (Show, Eq)

type Env = [(String, Type)]

env :: Env
env = 	[("+", FunType IntType (FunType IntType IntType))
		,("-", FunType IntType (FunType IntType IntType))
		,("*", FunType IntType (FunType IntType IntType))
		,("/", FunType IntType (FunType IntType IntType))
		,("&&", FunType BoolType (FunType BoolType BoolType))
		,("||",  FunType BoolType (FunType BoolType BoolType))
		,("<", FunType IntType (FunType IntType BoolType))
		,(">", FunType IntType (FunType IntType BoolType))
		,("yolo", IntType)
		,("swag", BoolType)
		,("hello", FunType IntType BoolType)
		]

typeOf :: Env -> Expr -> Type
typeOf _ 	(Const _)					= IntType
typeOf _ 	(Boolean _)					= BoolType
typeOf es	(Var x)						= case maybeType of
											Nothing -> error "variable bestaat niet"
											Just (_, ty) -> ty
	where maybeType = find (\(s, t) -> s == x) es
typeOf es (BinOp op e1 e2) 				= case maybeType of
											Nothing -> error ("geen binary operator '" ++ op ++ "'' bekend met type " ++ (show type1) ++ " -> " ++ (show type2) ++ " -> a")
											Just (_, ty) -> ty
	where
		type1 = typeOf es e1
		type2 = typeOf es e2
		maybeType = find (\(s, FunType t1 (FunType t2 tresult)) -> s == op && t1 == type1 && t2 == type2) es
typeOf es (App e1 e2)					| type2 == tArg		= tRes
										| otherwise			= error (show e2 ++ " heeft type " ++ (show type2) ++ ", maar " ++ (show e1) ++ " heeft type " ++ (show $ typeOf es e1))
	where
		FunType tArg tRes 	= typeOf es e1
		type2 				= typeOf es e2
typeOf es (ITE e1 e2 e3)	| typeOf es e1 == BoolType			= if b then typeOf es e2 else typeOf es e3
							| otherwise 						= error (show e1 ++ " is niet van type Boolean")
	where
		Boolean b = e1

typeOf es (TwoTup e1 e2) 				= TupType [typeOf es e1, typeOf es e2]
typeOf es (ThreeTup e1 e2 e3)			= TupType [typeOf es e1, typeOf es e2, typeOf es e3]
typeOf es (Lambda t e)					= FunType t (typeOf es e)