import Data.List

--x is only evaluated once and cached under the surface.
--this works since Haskell is referentially transparent.
g x = x + x + x

f (x,y)	= x*y
f' x y	= x*y

--curry f == f'
--uncurry f' == f

--'zip' is just a special case of 'zipWith'
--the function being the tuple constructor
myzip xs ys = zipWith (,) xs ys

--adding two lists together
vectorAdd v1 v2	= zipWith (+) v1 v2

--adding two lists of lists together
matrixAdd m1 m2	= zipWith vectorAdd m1 m2

--transpose top-down (instead of left-right)
--(for every row, add it in front of the already transposed matrix)
transp []		= repeat []
transp (xs:xss)	= zipWith (:) xs (transpose xss)

--function composition
-- (f . g) x = f (g x)
squaresSmallerThen n = takeWhile ((< n) . (^2)) [1..]

--($) is the function application operation, but it binds weaker than the normal one.
--f (g x)  is the same as  (f . g) x  is the same as  f $ g x

--vector multipliciation (dot product)
dotProduct xs ys	= foldl (+) 0 $ zipWith (*) xs ys

--list comprehension (same can be achieved with map and filter)
evens xs	= [x | x <- xs, even x]

smallerXSthenYS xs ys = [(x, y) | x <- xs, y <- ys, x < y]

--permutations
perms []	= [[]]
perms xs	= [x:p | x <- xs, p <- perms (xs \\ [x])]

--lambda abstractions
--f x = x^2     <==>    f = \x -> x^2