f :: (Num a) => a -> a
f x = x*x + 1

g :: (Num a) => a -> a -> a
g x y = f x + y

fun :: (Ord a, Num a) => a -> a -> a
fun x y	| a>0 && b>0	= a + b
		| otherwise		= a * b
			where
				a = 3 * x + y
				b = 3 * x

fac :: (Eq a, Num a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)

mylength :: Num a => [b] -> a
mylength []		= 0
mylength (x:xs)	= 1 + mylength xs

total :: Num a => [a] -> a
total []		= 0
total (x:xs)	= x + total xs

mymap :: (a->b) -> [a] -> [b]
mymap f []		= []
mymap f (x:xs)	= f x : mymap f xs