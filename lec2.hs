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