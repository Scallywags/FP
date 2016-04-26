module Lab3 where

import FPPrac.Trees

--1

data Tree1a	= Leaf1a Int
			| Node1a Int Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n)		= RoseNode (show n) []
pp1a (Node1a n l r)	= RoseNode (show n) [pp1a l, pp1a r]


data Tree1b	= Leaf1b (Int, Int)
			| Node1b (Int, Int) Tree1b Tree1b

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b t)		= RoseNode (show t) []
pp1b (Node1b t l r)	= RoseNode (show t) [pp1b l, pp1b r]

data Tree1c	= Leaf1c
			| Node1c Int Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c Leaf1c			= RoseNode "" []
pp1c (Node1c n l r)	= RoseNode (show n) [pp1c l, pp1c r]

data Tree1d	= Leaf1d (Int, Int)
			| Node1d Tree1d Tree1d

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d t)		= RoseNode (show t) []
pp1d (Node1d l r)	= RoseNode "" [pp1d l, pp1d r]

--2

mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) 		= Leaf1a (f n)
mapTree f (Node1a n l r)	= Node1a (f n) (mapTree f l) (mapTree f r)

treeAdd :: Tree1a -> Int -> Tree1a
treeAdd t n 	= mapTree (+n) t

treeSquare :: Tree1a -> Tree1a
treeSquare t 	= mapTree (^2) t

addNode :: Tree1b -> Tree1a
addNode (Leaf1b (x, y))		= Leaf1a (x + y)
addNode (Node1b (x, y) l r)	= Node1a (x + y) (addNode l) (addNode r)

mapTree1b :: ((Int, Int) -> Int) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b (x, y))		= Leaf1a (f x y)
mapTree1b f (Node1b (x, y) l r)	= Node1a (f x y) (mapTree1b f l) (mapTree1b f r)

--3

