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
