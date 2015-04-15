import Control.Applicative
import System.IO

-- Professor Nunes-Harwitt's Monad definition, very similar to Cont
newtype K r a = K ((a->r) -> r)
unWrap (K f) = f

instance Monad (K r) where
    return v = K (\k->k v)
    m >>= f  = K (\k->(unWrap m) (\v->unWrap (f v) k))

-- Abort modified to take generic types
abortWith :: a -> (K a b)
abortWith v = K (\k->v)

-- Sums a list of Integer and returns a Maybe r in Continuation Passing Style
sumListCPS :: [Int] -> (Maybe Int->Maybe r) -> Maybe r
sumListCPS [] k = Nothing
sumListCPS [x] k = k (Just x)
sumListCPS (x:xs) k = sumListCPS xs (\v->k (Just (+x) <*> v)) -- variation to using fmap (+1) v

-- Uses Continuation Passing Style (CPS) to find the index of an element
index_a :: Eq a => a -> [a] -> (Maybe Int->Maybe r) -> Maybe r
index_a a []     k = Nothing
index_a a (x:xs) k 
    | a /= x    = index_a a xs (\v->k (fmap (+1) v))
    | otherwise = k (Just 0)

-- Use K Monad abstraction for CPS
index_b :: Eq a => a -> [a] -> K (Maybe Int) (Maybe Int)
index_b a []     = abortWith Nothing
index_b a (x:xs)
    | a /= x    = index_b a xs >>= \v -> return (fmap (+1) v)
    | otherwise = return (Just 0)

-- Use Maybe Monad with bind and return
index_c :: Eq a => a -> [a] -> Maybe Int
index_c a []      = Nothing
index_c a (x:xs)
    | a /= x    = index_c a xs >>= return . (+1)
    | otherwise = Just 0

-- Use Maybe Monad with do notation and return
index_d :: Eq a => a -> [a] -> Maybe Int
index_d a []      = Nothing
index_d a (x:xs)
    | a == x    = Just 0 -- switched order for better readability
    | otherwise = 
        do  v <- index_d a xs
            return (v+1)

-- Basic IO demo
meetAndGreet :: IO ()
meetAndGreet = 
    do  putStr "What is your name? "
        name <- getLine
        putStrLn ("Hello " ++ name ++ "!")

-- Take a list of Double and return the average
average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

-- Take a prompt and sentinel value and continue prompting for IO
-- from user until sentinel value is entered, return the resulting IO Double list
readDoubles :: String -> String -> IO [Double]
readDoubles prompt sentinel = readHelper prompt sentinel []
    where 
    readHelper :: String -> String -> [Double] -> IO ([Double])
    readHelper _ _ xs = 
        do  putStr prompt
            numStr <- getLine
            if sentinel /= numStr
              then readHelper prompt sentinel (read numStr : xs)
              else return (xs)
        
-- Interface for IO operations to provide a prompt and sentinel to readDoubles
-- and provide formatted output to user
interface :: IO ()
interface =
    do  nums <- readDoubles "Enter a number: " "done"
        putStr    "The average is "
        (putStrLn . show) (average nums)
        putStr    "The maximum is "
        (putStrLn . show) (maximum nums)
        putStr    "The minimum is "
        (putStrLn . show) (minimum nums)

-- Read contents from a file path and write them to a file path, a copy command
cp :: String -> String -> IO ()
cp fromFilePath toFilePath =
    do  contents <- readFile fromFilePath
        writeFile toFilePath contents
