toDigits :: Integer -> [Integer]
toDigits x	| x <= 0	= []
		| otherwise 	= helper x []
			where	helper x [] = helper (x `div` 10) (x `mod` 10 : [])
				helper 0 xs = xs
				helper x xs = helper (x `div` 10) (x `mod` 10 : xs) 

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ doubleEveryOtherBeginning (reverse x)

doubleEveryOtherBeginning :: [Integer] -> [Integer]
doubleEveryOtherBeginning (x:y:zs) 	= x : y*2 : doubleEveryOtherBeginning zs
doubleEveryOtherBeginning xs 		= xs

sumDigits :: [Integer] -> Integer
sumDigits [] 		= 0
sumDigits (x:xs)	= sumNumber x + sumDigits xs

sumNumber :: Integer -> Integer
sumNumber x	| x > 9  	= x `mod` 10 + sumNumber (x `div` 10)
		| otherwise	= x

validate :: Integer -> Bool
validate x 	= sumDigits (doubleEveryOther $ toDigits x) `mod` 10 == 0

