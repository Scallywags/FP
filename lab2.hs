module Lab2 where

import Data.List

--1

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ []		= []
myfilter p (x:xs)	| p x		= x:myfilter p xs
					| otherwise	= myfilter p xs

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ initial []		= initial
myfoldl f initial (x:xs)	= myfoldl f (f initial x) xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ initial []		= initial
myfoldr f initial (x:xs)	= f x (myfoldr f initial xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith _ [] _			= []
myzipWith _ _ []			= []
myzipWith f (x:xs) (y:ys)	= f x y : myzipWith f xs ys

--2

type Person = (String, Int, Char, String)

database :: [Person]
database	=	[ ("Jan", 20, 'M', "Rijssen")
				, ("Jeroen", 20, 'M', "Enschede")
				, ("Lindsay", 19, 'F', "Enschede")
				, ("Marieke", 34, 'F', "Oldenzaal")
				]
--getters

getname :: Person -> String
getname (name, _, _, _) = name

getage :: Person -> Int
getage (_, age, _, _) = age

getsex :: Person -> Char
getsex (_, _, sex, _) = sex

getpor :: Person -> String
getpor (_, _, _, por) = por

--setters

setname :: Person -> String -> Person
setname (_, age, sex, por) name	= (name, age, sex, por)

setage :: Person -> Int -> Person
setage (name, _, sex, por) age	= (name, age, sex, por)

setsex :: Person -> Char -> Person
setsex (name, age, _, por) sex	= (name, age, sex, por)

setpor :: Person -> String -> Person
setpor (name, age, sex, _) por	= (name, age, sex, por)

--actual stuff

incAgeRec :: [Person] -> Int -> [Person]
incAgeRec [] _		= []
incAgeRec (p:ps) n	= setage p (getage p + n):incAgeRec ps n

incAgeLiCo :: [Person] -> Int -> [Person]
incAgeLiCo ps n	= [setage p (getage p + n) | p <- ps]

incAgeHiOr :: [Person] -> Int -> [Person]
incAgeHiOr ps n = map (\p -> setage p (getage p + n)) ps

agedWomenRec :: [Person] -> [String]
agedWomenRec []		= []
agedWomenRec (p:ps)	| 30 <= age && age < 40 && getsex p == 'F'	= getname p:agedWomenRec ps
					| otherwise									= agedWomenRec ps
						where age = getage p

agedWomenLiCo :: [Person] -> [String]
agedWomenLiCo ps	= [getname p | p <- ps, 30 <= getage p && getage p < 40 && getsex p == 'F']

agedWomenHiOr :: [Person] -> [String]
agedWomenHiOr ps	= map getname $ filter (\p -> 30 <= getage p && getage p < 40 && getsex p == 'F') ps


yieldage :: String -> [Person] -> Int
yieldage name db 	= case personM of 
								Just p	-> getage p
					  			Nothing	-> error ("No person found with name " ++ name)
						where personM = find (\p -> name == getname p) db

sortpersons :: [Person] -> [Person]
sortpersons = map (\(a, b, c, d) -> (b, a, c, d)) . sort . map (\(a, b, c, d) -> (b, a, c, d))

--3

sieve' :: [Integer] -> [Integer]
sieve' []		= []
sieve' (x:xs)	= x : sieve' (filter  ((/=0).(`mod` x)) xs)

sieve :: [Integer]
sieve = sieve' [2..]

isPrime :: Integer -> Bool
isPrime n = n `elem` (takeWhile (<= n) sieve)

nPrimes :: Int -> [Integer]
nPrimes n = take n sieve

primesLTN :: Integer -> [Integer]
primesLTN n = takeWhile (<n) sieve

dividers :: Integer -> [Integer]
dividers m 	= [n | n <- [1..m], m `mod` n == 0]

isPrimeAlt :: Integer -> Bool
isPrimeAlt n = dividers n == [1, n]

--4

pyth :: Int -> [(Int, Int, Int)]
pyth n 	= [(a, b, c) | a <- [1..(n-1)], b <- [1..(n-1)], c <- [1..(n-1)], a^2+b^2 == c^2]

--DOES NOT WORK
pythtest = [(a, b, c) | a <- [1..], b <- [1..], c <- [1..], a^2+b^2 == c^2]

pyth' :: Int -> [(Int, Int, Int)]
pyth' n = [(a, b, c) | a <- [1..(n-1)], b <- [1..a], c <- [1..(n-1)], a^2+b^2 == c^2, gcd a b == 1]

--5

increasing :: Ord a => [a] -> Bool
increasing xs = and $ zipWith (>) (tail xs) xs

mean :: (Integral a) => [a] -> a
mean xs = fromIntegral (sum xs) `div` fromIntegral (length xs)

weakIncr :: (Integral a) => [a] -> Bool
weakIncr []		= True
weakIncr [x]	= True
weakIncr xs		= (last xs > mean (init xs)) && weakIncr (init xs)

--6

sublist :: Eq a => [a] -> [a] -> Bool
sublist = isInfixOf

partsublist :: Eq a => [a] -> [a] -> Bool
partsublist [] _			= True
partsublist _ []			= False
partsublist (x:xs) (y:ys)	| x == y 	= partsublist xs ys
							| otherwise = partsublist (x:xs) ys

--7

bubble :: Ord a => [a] -> [a]
bubble []			= []
bubble [x]			= [x]
bubble (x:x':xs)	| x < x' 	= x:bubble (x':xs)
					| otherwise = x':bubble (x:xs)

bsort :: Ord a => [a] -> [a]
bsort xs	| xs == ys = xs
			| otherwise = bsort ys
				where ys = bubble xs

mmsort :: Ord a => [a] -> [a]
mmsort []	= []
mmsort [x]	= [x]
mmsort xs	= [smallest] ++ mmsort rest ++ [largest]
				where
					smallest = minimum xs
					largest = maximum xs
					rest = xs \\ [smallest, largest]

ins :: Ord a => a -> [a] -> [a]
ins x []		= [x]
ins x (y:ys)	| x < y 	= x:y:ys
				| otherwise = y:ins x ys

isort :: Ord a => [a] -> [a]
isort []		= []
isort (x:xs)	= ins x (isort xs)
--TODO how to do with a fold?
--isort xs		= foldl ins [] xs ???


merge :: Ord a => [a] -> [a] -> [a]
merge xs []			= xs
merge [] ys			= ys
merge (x:xs) (y:ys) | x < y 	= x:merge xs (y:ys)
					| otherwise	= y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []	= []
msort [x]	= [x]
msort xs	= merge (msort left) (msort right)
	where (left, right) = splitAt (length xs `div` 2) xs

qsort :: Ord a => [a] -> [a]
qsort []		= []
qsort (x:xs)	= qsort smaller ++ [x] ++ qsort bigger
	where (smaller, bigger) = partition (<x) xs
