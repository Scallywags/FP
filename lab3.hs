module Lab3 where

import FPPrac.Trees

--1

data Tree1a	= Leaf1a Int
			| Node1a Int Tree1a Tree1a
				deriving (Show, Eq)

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n)		= RoseNode (show n) []
pp1a (Node1a n l r)	= RoseNode (show n) [pp1a l, pp1a r]


data Tree1b	= Leaf1b (Int, Int)
			| Node1b (Int, Int) Tree1b Tree1b
				deriving (Show, Eq)

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b t)		= RoseNode (show t) []
pp1b (Node1b t l r)	= RoseNode (show t) [pp1b l, pp1b r]

data Tree1c	= Leaf1c
			| Node1c Int Tree1c Tree1c
				deriving (Show, Eq)

pp1c :: Tree1c -> RoseTree
pp1c Leaf1c			= RoseNode "" []
pp1c (Node1c n l r)	= RoseNode (show n) [pp1c l, pp1c r]

data Tree1d	= Leaf1d (Int, Int)
			| Node1d [Tree1d]
				deriving (Show, Eq)

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d t)		= RoseNode (show t) []
pp1d (Node1d ts)	= RoseNode "" (map pp1d ts)

--2

mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) 		= Leaf1a (f n)
mapTree f (Node1a n l r)	= Node1a (f n) (mapTree f l) (mapTree f r)

treeAdd :: Tree1a -> Int -> Tree1a
treeAdd t n 	= mapTree (+n) t

treeSquare :: Tree1a -> Tree1a
treeSquare t 	= mapTree (^2) t

addNode :: Tree1b -> Tree1a
addNode t = mapTree1b (\(x, y) -> x + y) t

mapTree1b :: ((Int, Int) -> Int) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b t)		= Leaf1a (f t)
mapTree1b f (Node1b t l r)	= Node1a (f t) (mapTree1b f l) (mapTree1b f r)

--3

binMirror :: Tree1a -> Tree1a
binMirror t@(Leaf1a _)		= t
binMirror (Node1a n l r)	= Node1a n (binMirror r) (binMirror l)

binMirror1d :: Tree1d -> Tree1d
binMirror1d (Leaf1d (x, y))	= Leaf1d (y, x)
binMirror1d (Node1d ts)		= Node1d (map binMirror1d (reverse ts))

--4

insertTree :: Int -> Tree1c -> Tree1c
insertTree n (Leaf1c) 			= Node1c n Leaf1c Leaf1c
insertTree n (Node1c x l r)		| n < x		= Node1c x (insertTree n l) r
								| otherwise	= Node1c x l (insertTree n r)

makeTree :: [Int] -> Tree1c
makeTree xs		= foldr insertTree Leaf1c xs

makeTreeNoobish :: [Int] -> Tree1c
makeTreeNoobish []		= Leaf1c
makeTreeNoobish (x:xs)	= insertTree x (makeTreeNoobish xs)

makeList :: Tree1c -> [Int]
makeList Leaf1c			= []
makeList (Node1c n l r)	= (makeList l) ++ [n] ++ (makeList r)

tsort :: [Int] -> [Int]
tsort = makeList . makeTree

tsortinv :: Tree1c -> Tree1c
tsortinv = makeTree . makeList

--5

subTreeAt :: Int ->  Tree1c -> Tree1c
subTreeAt n Leaf1c				= error ("Oops 404! " ++ (show n) ++ " was not found!")
subTreeAt n t@(Node1c x l r)	| n == x 	= t
								| n < x 	= subTreeAt n l
								| otherwise	= subTreeAt n r

--6

cutOffAt :: Int -> Tree1c -> Tree1c
cutOffAt 0 _				= Leaf1c
cutOffAt _ Leaf1c			= Leaf1c
cutOffAt n (Node1c x l r) 	= Node1c x (cutOffAt (n-1) l) (cutOffAt (n-1) r)

--7

type Path = String

replace :: Path -> Int -> Tree1a -> Tree1a
replace [] n (Leaf1a _)				= Leaf1a n
replace _ _ t@(Leaf1a _)			= t
replace [] n (Node1a _ l r)			= Node1a n l r
replace ('l':ds) n (Node1a x l r)	= Node1a x (replace ds n l) r
replace ('r':ds) n (Node1a x l r)	= Node1a x l (replace ds n r)

subTree :: Path -> Tree1a -> Tree1a
subTree []	t 						= t
subTree _	(Leaf1a _)				= error "Path too long!"
subTree ('l':ds) (Node1a x l r)		= subTree ds l
subTree ('r':ds) (Node1a x l r)		= subTree ds r

-- extra

leftmostLeafPath :: Tree1a -> Path
leftmostLeafPath t@(Leaf1a _)		= ""
leftmostLeafPath (Node1a _ l _)		= 'l':leftmostLeafPath l

rightmostLeafPath :: Tree1a -> Path
rightmostLeafPath t@(Leaf1a _)		= ""
rightmostLeafPath (Node1a _ _ r)	= 'r':rightmostLeafPath r

treeElem :: Tree1a -> Tree1a -> Bool
treeElem (Leaf1a n) (Leaf1a x)		= n == x
treeElem (Node1a _ _ _) (Leaf1a _)	= False
treeElem tree t@(Node1a _ l r)		= tree == t || tree `treeElem` l || tree `treeElem` r

calcPath :: Tree1a -> Tree1a -> Path
calcPath leaf t@(Leaf1a _)			= "" -- we just assume we don't get here when the leaves are not the same.
calcPath leaf t@(Node1a _ l r)		| leaf `treeElem` l 	= 'l':calcPath leaf l
									| leaf `treeElem` r 	= 'r':calcPath leaf r
									| leaf == t 			= ""
									| otherwise = error (show t ++ " does not contain " ++ show leaf)

leftNeighbour :: Tree1a -> Tree1a -> Path
leftNeighbour leaf tree = leftNeighbour' leaf tree path
	where
		path = calcPath leaf tree
		leftNeighbour' lf (Node1a _ l _) ('l':ds)	= 'l':leftNeighbour' lf l ds
		leftNeighbour' _ (Node1a _ l _) ('r':_)		= 'l':rightmostLeafPath l
		leftNeighbour' _ (Leaf1a _) _				= error (show leaf ++ " is the leftmost leaf in " ++ show tree)

rightNeighbour :: Tree1a -> Tree1a -> Path
rightNeighbour leaf tree = rightNeighbour' leaf tree path
	where
		path = calcPath leaf tree
		rightNeighbour' lf (Node1a _ _ r) ('r':ds)	= 'r':rightNeighbour' lf r ds
		rightNeighbour' _ (Node1a _ _ r) ('l':ds)	= 'r':leftmostLeafPath r
		rightNeighbour' _ (Leaf1a _) _				= error (show leaf ++ " is the rightmost leaf in " ++ show tree)

--8

depth :: Tree1c -> Int
depth Leaf1c			= 0
depth (Node1c _ l r)	= 1 + max (depth l) (depth r)

isBalanced :: Tree1c -> Bool
isBalanced Leaf1c			= True
isBalanced (Node1c _ l r)	= abs (depth l - depth r) <= 1 && isBalanced l && isBalanced r

makeBalancedTree :: [Int] -> Tree1c
makeBalancedTree []			= Leaf1c
makeBalancedTree xs			= Node1c top (makeBalancedTree left) (makeBalancedTree right)
								where (left, (top:right)) = splitAt (length xs `div` 2) xs

balance :: Tree1c -> Tree1c
balance = makeBalancedTree . makeList

checkBalanced :: Int -> Bool
checkBalanced n = isBalanced (balance (makeTree [1..n])) && checkBalanced (n-1)