module Lab5 where

import Data.List
import FPPrac.Trees

type Stack  = [Int]
type Heap   = [Int]

data Op     = Add | Mul | Sub
            deriving (Show, Eq)

data Instr  = PushConst Int
            | PushAddr Int
            | Store Int
            | Load Int
            | PushPC
            | EndRep
            | Calc Op
            | EndProg
            deriving (Show, Eq)

data Tick = Tick deriving (Show, Eq)

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''
          deriving (Show, Eq)

--2

codeGen2 :: Expr -> [Instr]
codeGen2 e 	= codeGen2' e ++ [EndProg]

codeGen2' :: Expr -> [Instr]
codeGen2' (Const x)				= [PushConst x]
codeGen2' (BinExpr op e1 e2)	= codeGen2' e2 ++ codeGen2' e1 ++ [Calc op]

--3

ppExpr :: Expr -> RoseTree
ppExpr (Const x)          = RoseNode (show x) []
ppExpr (BinExpr op e1 e2) = RoseNode (show op) [ppExpr e1, ppExpr e2]

--5

type Variable = Int                     -- the Address in the heap
data Stmnt  = Assign Variable Expr
			| Repeat Expr [Stmnt]
			deriving (Show, Eq)

codeGen5 :: Stmnt -> [Instr]
codeGen5 (Assign addr e)	= codeGen2' e ++ [Store addr]
codeGen5 (Repeat e stmnts)	= [PushPC, PushConst (length stmnts)] ++ (concatMap codeGen5 stmnts) ++ [EndRep]

--6

class CodeGen c where
	codeGen :: c -> [Instr]

instance CodeGen Expr where
	codeGen = codeGen2'

instance CodeGen Stmnt where
	codeGen = codeGen5

class RT t where
	ppTree :: t -> RoseTree

instance RT Expr where
	ppTree = ppExpr

instance RT Stmnt where
	ppTree (Assign addr e)  	= RoseNode "Assign" [RoseNode (show addr) [], ppTree e]
	ppTree (Repeat e stmnts)	= RoseNode "Repeat" (ppTree e : map ppTree stmnts)

--7

