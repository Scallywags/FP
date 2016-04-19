module Lab1 where

import Data.Char

--1

f :: Num a => a -> a
f x = 2 * x ^ 2 + 3 * x - 5

--2

code :: Char -> Char
code x	| isUpper x	= chr ((ord x - 65 + 3) `mod` 26 + 65)
		| otherwise	= chr ((ord x - 97 + 3) `mod` 26 + 97)

code' :: Char -> Int -> Char
code' x n = chr ((ord x - offset + n) `mod` 26 + offset)
			where
				offset	| isUpper x	= 65
						| otherwise	= 97

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
mymaximum _			= error "no maximum element in empty list"

myzip :: [a] -> [b] -> [(a,b)]
myzip xs []			= []
myzip [] ys			= []
myzip (x:xs) (y:ys)	= (x, y):myzip xs ys