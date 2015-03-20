toDigits :: Integer -> [Integer]
toDigits x = 	if x <= 0 then []
		else helper x []
			where	helper x [] = helper (div x 10) (mod x 10 : [])
				helper 0 xs = xs
				helper x xs = helper (div x 10) (mod x 10 : xs)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:[]) = 	[x]
doubleEveryOther (x:y:[]) =	x * 2 : y : []
doubleEveryOther (x:y:zs) = 	double (reverse(x:y:zs))
				where double (x:y:zs) = reverse $ x : y*2 : reverse(doubleEveryOther (reverse zs))


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' x = reverse $ doubleEveryOtherBeginning (reverse x)

doubleEveryOtherBeginning :: [Integer] -> [Integer]
doubleEveryOtherBeginning [] = []
doubleEveryOtherBeginning [x] = [x]
doubleEveryOtherBeginning (x:y:[]) = x:y*2
doubleEveryOtherBeginning (x:y:zs) = x:y*2:doubleEveryOtherBeginning zs
