toDigits :: Integer -> [Integer]
toDigits x = 	if x <= 0 then []
		else helper x []
			where	helper x [] = helper (div x 10) (mod x 10 : [])
				helper 0 xs = xs
				helper x xs = helper (div x 10) (mod x 10 : xs)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:[]) 	= 	[x]
doubleEveryOther (x:y:[]) 	=	x * 2 : y : []
doubleEveryOther (x:y:zs) 	= 	double (reverse(x:y:zs))
					where double (x:y:zs) = reverse $ x : y*2 : reverse(doubleEveryOther (reverse zs))


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' x = reverse $ doubleEveryOtherBeginning (reverse x)

doubleEveryOtherBeginning :: [Integer] -> [Integer]
doubleEveryOtherBeginning (x:y:zs) 	= x : y*2 : doubleEveryOtherBeginning zs
doubleEveryOtherBeginning xs 		= xs

sumDigits :: [Integer] -> Integer
sumDigits [] 		= 0
sumDigits (x:y:[]) 	= x `div` 10 + x `mod` 10 + y
sumDigits (x:y:xs) 	= x `div` 10 + x `mod` 10 + y + sumDigits xs

sumNumber :: Integer -> Integer
sumNumber x	| x > 9  	= x `mod` 10 + sumNumber (x `div` 10)
		| otherwise	= x
