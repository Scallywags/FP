module Tentamen where

import Data.List

--1

--a
perfect :: Int -> [Int]
perfect m = [n | n <- [1..(m-1)], isPerfect n] where
    dividers n  = [x | x <- [1..(n-1)], n `mod` x == 0]
    isPerfect n = n == (sum $ dividers n)

--b
jollyHO :: [Int] -> Bool
jollyHO xs = difs `elem` permutations [1..length xs - 1]
    where difs = map abs (zipWith (-) xs (tail xs))

jollyR :: [Int] -> Bool
jollyR xs   = jollyR' xs []

jollyR' :: [Int] -> [Int] -> Bool
jollyR' (x:x':xs) found | dif `elem` found  = False
                        | otherwise         = jollyR' (x':xs) (dif:found)
    where dif = abs (x-x')
jollyR' _ _             = True

--c
addRowsHO :: Num a => [[a]] -> [a]
addRowsHO = map sum

addRowsLC :: Num a => [[a]] -> [a]
addRowsLC m = [sum row | row <- m]

addRowsR :: Num a => [[a]] -> [a]
addRowsR []         = []
addRowsR (xs:xss)   = sum xs:addRowsR xss

addColumnsHO :: Num a => [[a]] -> [a]
addColumnsHO = addRowsHO . transpose

addColumnsLC :: Num a => [[a]] -> [a]
addColumnsLC m = [sum column | column <- transpose m]

addColumnsR :: Num a => [[a]] -> [a]
addColumnsR []      = []
addColumnsR m = sum xs:addColumnsR xss
    where (xs:xss) = transpose m

--d
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldl (\l x -> (l ++ [f x])) [] xs

--alternatively using foldr: myMap f xs = foldr (\x -> (f x:)) [] xs

--2

--a
data Tree a = Node a [Tree a]
            | Leaf

type IntTree = Tree Int
type CBTree = Tree (Char, Bool)

--b
treeMax :: IntTree -> Int
treeMax (Node n []) = n
treeMax (Node n ts) = max n (maximum $ map treeMax nodes) where
    isNode (Node _ _)   = True
    isNode Leaf         = False
    nodes = filter isNode ts
treeMax Leaf        = error "no maximum of emty tree"

--c
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf          = Leaf
mapTree f (Node x ts)   = Node (f x) (map (mapTree f) ts)

--d
treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Leaf         = False
treeElem x (Node y ts)  = x == y || any (treeElem x) ts

yieldPath :: Eq a => a -> Tree a -> [Int]
yieldPath x (Node y ts)
    | x == y    = []
    | otherwise = index : yieldPath x (ts!!index)
    where 
        maybeIndex = findIndex (treeElem x) ts
        index = case maybeIndex of
            Just i -> i
            Nothing -> error "element not found in subtrees"
yieldPath _ Leaf    = error "element not in empty tree"

--4

--a
data Press  = N Int
            | O (Int -> Int -> Int)
            | IS
type Input  = [Press]

--b
type Memory = (Maybe Int, Maybe (Int -> Int -> Int), Maybe Int)

eval :: Input -> [Int]
eval input = calc (Nothing, Nothing, Nothing) input

calc :: Memory -> Input -> [Int]
calc (Nothing, opM, oldN) (N n:ps)      = n:calc (oldN, opM, Just n) ps
calc (Nothing, _, Just n) (O o:ps)      = n:calc (Nothing, Just o, Just n) ps
calc (Just n1, _, Just n2) (O o:ps)     = res:calc (Just n2, Just o, Just res) ps where res = o n1 n2
calc (Nothing, _, Just n) [IS]          = [n]
calc (Just n1, Just o, Just n2) [IS]    = [o n1 n2]
calc _ _                                = error "Invalid Input"

--c
data Press2 = Num Int
            | BinOp (Int -> Int -> Int)
            | UnOp (Int -> Int)
            | Eval
type Input2 = [Press2]

eval2 :: Input2 -> [Int]
eval2 input = calc2 (Nothing, Nothing, Nothing) input

calc2 :: Memory -> Input2 -> [Int]
calc2 (Nothing, opM, oldN) (Num n:ps)           = n:calc2 (oldN, opM, Just n) ps
calc2 (Nothing, _, Just n) (BinOp o:ps)         = n:calc2 (Nothing, Just o, Just n) ps
calc2 (Just n1, _, Just n2) (BinOp o:ps)        = res:calc2 (Just n2, Just o, Just res) ps where res = o n1 n2
calc2 (_, opM, Just n) (UnOp o:ps)              = res:calc2 (Just n, opM, Just res) ps where res = o n
calc2 (Nothing, _, Just n) [Eval]               = [n]
calc2 (Just n1, Just o, Just n2) [Eval]         = [o n1 n2]
calc2 _ _                                       = error "Invalid Input"