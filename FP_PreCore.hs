module FP_PreCore where

{-
Elementary concept of processor;
List of instructions is considered as "input", i.e., instructions are not in "memory"
-}

data Op    = Add | Mul | Sub
           deriving Show

data Instr = Push Int
           | Calc Op
           deriving Show

-- ========================================================================
-- Processor functions

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)

core stack instr = case instr of

                     Push n   -> n : stack

                     Calc op  -> v : stack'
                              where
                                x:y:stack' = stack
                                v          = alu op y x

-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

prog = [ Push 2
       , Push 10
       , Calc Mul
       , Push 3
       , Push 4
       , Push 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , Push 12
       , Push 5
       , Calc Add
       , Calc Mul
       ]

test0 = putStr $ unlines $ map show
           $ foldl core [] prog

test1 = putStr $ unlines $ map show
           $ scanl core [] prog

