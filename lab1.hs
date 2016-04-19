module Lab1 where

import Data.Char

--1

f :: Num a => a -> a
f x = 2 * x * x + 3 * x - 5

--2

code :: Char -> Char
code x	| isUpper x	= chr ((ord x - 65 + 3) `mod` 26 + 65)
		| isLower x	= chr ((ord x - 97 + 3) `mod` 26 + 97)
		| otherwise	= x

code' :: Char -> Int -> Char
code' x n	| isUpper x	= chr ((ord x - 65 + n) `mod` 26 + 65)
			| isLower x	= chr ((ord x - 97 + n) `mod` 26 + 97)
			| otherwise	= x

--3

interest :: Float -> Float -> Int -> Float
interest money _		0	= money
interest money rate years	= money * rate * interest 1 rate (years-1)

--4

discr :: Float -> Float -> Float -> Float
discr a b c = b*b - 4*a*c

root :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float
root f a b c = ((-b) `f` (sqrt d)) / (2*a)
				where
					dis = discr a b c
					d	| dis < 0		= error "negative discriminant"
						| otherwise		= dis

root1 :: Float -> Float -> Float -> Float
root1 a b c = root (+) a b c

root2 :: Float -> Float -> Float -> Float
root2 a b c = root (-) a b c

--5

extrX :: (Fractional a, Num a) => a -> a -> b -> a
extrX a b _		= (-b) / (2*a)

extrY ::(Fractional a, Num a) => (a -> a -> a -> a -> a) -> a -> a -> a -> a
extrY f a b c	= f (extrX a b c) a b c

--6

mylength :: [a] -> Int
mylength []		= 0
mylength (x:xs)	= 1 + mylength xs

mysum :: Num a => [a] -> a
mysum [] 		= 0
mysum (x:xs)	= x + mysum xs

myreverse :: [a] -> [a]
myreverse []		= []
myreverse (x:xs)	= (myreverse xs) ++ [x]

mytake :: Int -> [a] -> [a]
mytake 0 _		= []
mytake n []		= []
mytake n (x:xs)	= x : mytake (n-1) xs

myelem :: Eq a => a -> [a] -> Bool
myelem _ []		= False
myelem y (x:xs)	| y == x 	= True
				| otherwise	= myelem y xs

myconcat :: [[a]] -> [a]
myconcat []			= []
myconcat (xs:xss)	= xs ++ myconcat xss

mymaximum :: Ord a => [a] -> a
mymaximum [x]		= x
mymaximum (x:x':xs)	= mymaximum ((max x x'):xs)
mymaximum []		= error "no maximum element in empty list"

myzip :: [a] -> [b] -> [(a,b)]
myzip xs []			= []
myzip [] ys			= []
myzip (x:xs) (y:ys)	= (x, y):myzip xs ys

--7

r :: Num a => a -> a -> [a]
r x d = iterate (+d) x

rl :: Num a => Int -> a -> a -> a
rl n x d	= r x d !! n

total :: Num a => Int -> Int -> [a] -> a
total i j xs = mysum $ take (j-i+1) (drop i xs)

--8

allEqual :: Eq a => [a] -> Bool
allEqual (x:x':xs)	= x == x' && allEqual (x':xs)
allEqual _			= True

isAS :: (Eq a, Num a) => [a] -> Bool
isAS (x:x':xs)	= allEqual $ differences (x:x':xs)
					where differences (y:y':ys)	= y' - y : differences (y':ys)
isAS _			= True

--9

type Matrix a = [[a]]

equalRowLengths :: Matrix a -> Bool
equalRowLengths m = allEqual (map length m)

rowTotals :: Num a => Matrix a -> [a]
rowTotals m = map mysum m

transpose :: Matrix a -> Matrix a
transpose []	= []
transpose m		= (map head m) : transpose (tail m)

columnTotals :: Num a => Matrix a -> [a]
columnTotals m = rowTotals $ transpose m