--Author: Ryan Castner

import Series

sinPowSeD :: PowSe Double
sinPowSeD = let (PowSe xs) = integrate cosPowSeD
            in PowSe(0:xs)

cosPowSeD :: PowSe Double
cosPowSeD = let (PowSe xs) = integrate (negate sinPowSeD)
            in PowSe(1:xs)

sinPowSeR :: PowSe Rational
sinPowSeR = let (PowSe xs) = integrate cosPowSeR
            in PowSe(0:xs)

cosPowSeR :: PowSe Rational
cosPowSeR = let (PowSe xs) = integrate (negate sinPowSeR)
            in PowSe(1:xs)

mySine :: Floating a => a -> a
mySine x = 3 * sin (x/3) - 4 * (sin(x/3) ^3)

sub :: Num a => [a] -> [a] -> [a]
sub []     []     = []
sub l      []     = l
sub []     l      = map negate l
sub (x:[]) (y:[]) = [x-y]
sub (x:xs) (y:ys) = (x-y) : sub xs ys

scaleList :: Fractional a => a -> [a] -> [a]
scaleList _ []     = []
scaleList x (y:ys) = (x*y) : scaleList x ys

subScale :: Fractional a => [a] -> [a] -> [a] 
subScale []     _       = error "Empty list provided"
subScale _      []      = error "Empty list provided"
subScale (x:xs) (y:ys)  = sub ys (scaleList (y/x) xs)

findNot :: Eq a => a -> [[a]] -> ([a], [[a]])
findNot a lst =  findNot' a lst []
    where
                 findNot' :: Eq a => a -> [[a]] -> [[a]] -> ([a], [[a]])
                 findNot' a (x:xs) ys = if a /= head x then (x,reverse (xs++ys))
                                  else findNot' a xs ([x]++ys) 

nonZeroFirst :: (Num a, Eq a) => [[a]] -> [[a]]
nonZeroFirst lst = fst tuple : snd tuple
    where tuple = findNot 0 lst

triangulate :: (Fractional a, Eq a) => [[a]] -> [[a]]
triangulate []          = error "Empty list provided"
triangulate (x1:[])     = [x1]
triangulate (x1:x2:[])  = [x1] ++ ([subScale x1 x2])
triangulate (x1:x2:xs)  = if length x1 > length x2 then [x1] ++ triangulate (x2:xs)
                          else triangulate ([x1] ++ (nonZeroFirst ([subScale x1 x2] ++ xs)))

dot :: Num a => [a] -> [a] -> a
dot (x:xs) []     = error "Lists not equal length"
dot []     (y:ys) = error "Lists not equal length"
dot []     []     = 0
dot (x:xs) (y:ys) = x*y + dot xs ys

solveLine :: Fractional a => [a] -> [a] -> a
solveLine (x:y:[])  []   = y/x
solveLine (x:xs)    lst  = (last xs - dot (init xs) lst) / x

solveTriangular :: Fractional a => [[a]] -> [a]
solveTriangular (x:[])  = [solveLine x []]
solveTriangular l       = solveTriangularSol (tail rlst) ([solveLine (head rlst) []])
    where rlst = reverse l

solveTriangularSol :: Fractional a => [[a]] -> [a] -> [a]
solveTriangularSol (x:[]) a = [solveLine x a]
solveTriangularSol (x:xs) a = sol ++ solveTriangularSol xs sol
    where sol = a ++ [solveLine x a]

solveSystem :: (Fractional a, Eq a) => [[a]] -> [a]
solveSystem x = solveTriangular (triangulate x)
