module Units where

import Data.List
import Data.Char

decimalPrefixes :: [String]
decimalPrefixes = 
  ["yocto- = 1e-24 = y-"
  ,"zepto- = 1e-21 = z-"
  ,"atto- =  1e-18 = a-"
  ,"femto- = 1e-15 = f-"
  ,"pico- =  1e-12 = p-"
  ,"nano- =  1e-9  = n-"
  ,"micro- = 1e-6  = u-"
  ,"milli- = 1e-3  = m-"
  ,"centi- = 1e-2  = c-"
  ,"deci- =  1e-1  = d-"
  ,"    - =  1e0   -  -"
  ,"deca- =  1e+1  = da-"
  ,"hecto- = 1e2   = h-"
  ,"kilo- =  1e3   = k-"
  ,"mega- =  1e6   = M-"
  ,"giga- =  1e9   = G-"
  ,"tera- =  1e12  = T-"
  ,"peta- =  1e15  = P-"
  ,"exa- =   1e18  = E-"
  ,"zetta- = 1e21  = Z-"
  ,"yotta- = 1e24  = Y-"
  ]

data MetricBaseUnit = Meter | Gram | Second | Liter | Kelvin | Mole | Ampere | Candela deriving (Show, Eq)

data Unit = M Prefix MetricBaseUnit       
type Prefix = String
type Power  = String

data Measurement = MetricMeasurement Double Unit

getPrefix :: String -> Prefix
getPrefix str = prefix where (prefix, abbr) = parsePrefix (getPrefixLine (take 3 (reverse (takeWhile (/= ' ') str))) decimalPrefixes) decimalPrefixes

strToMeasurement :: String -> Measurement
strToMeasurement str = MetricMeasurement (parseValue str) (parseUnit str)

parseUnit :: String -> Unit
parseUnit str = M prefix (parseBase str) where (prefix, abbr) = parsePrefix (getPrefixLine (take 3 (dropWhile (`notElem` ['a'..'z']) str)) decimalPrefixes) decimalPrefixes 

parseBase :: String -> MetricBaseUnit
parseBase str
    | "meter" `isInfixOf` str   = Meter
    | "gram" `isInfixOf` str    = Gram
    | "ampere" `isInfixOf` str  = Ampere
    | "candela" `isInfixOf` str = Candela
    | "liter" `isInfixOf` str   = Liter
    | "mole" `isInfixOf` str    = Mole
    | "kelvin" `isInfixOf` str  = Kelvin
    | "second" `isInfixOf` str  = Second

parseValue :: String -> Double
parseValue str = read (takeWhile (`elem` (map intToDigit [0..9])) (dropWhile (`notElem` (map intToDigit [0..9])) str)) :: Double

getPrefixLine :: String -> [String] -> String
getPrefixLine str (x:xs)
    | str `isPrefixOf` x  = x
    | otherwise           = getPrefixLine str xs

parseToHyphen :: String -> String
parseToHyphen (x:xs)
    | x == '-'  = []
    | otherwise = [x] ++ parseToHyphen xs

parsePrefix :: String -> [String] -> (Prefix, String)
parsePrefix str (x:xs) 
    | str `isPrefixOf` x  = ((parseToHyphen x), parseAbbr (drop (length str) x))
    | otherwise           = parsePrefix str xs

parseAbbr :: String -> String
parseAbbr str = drop 1 (parseHelper (reverse str))
    where parseHelper (x:xs)
              | x == ' '  = []
              | otherwise = [x] ++ parseHelper xs

unitStr :: Unit -> String
unitStr u@(M prefix base) = prefix ++ lowerUnit u

lowerUnit :: Unit -> String
lowerUnit (M prefix base) = map toLower (show base)

unitAbbr :: Unit -> String
unitAbbr u@(M prefix base) = snd (parsePrefix prefix decimalPrefixes) ++ [head (lowerUnit u)]

unitPrefix :: Unit -> Prefix
unitPrefix (M prefix base) = prefix

unitBase :: Unit -> MetricBaseUnit
unitBase (M prefix base) = base

parsePower :: String -> String
parsePower prefix = let line = (getPrefixLine prefix decimalPrefixes) in reverse (dropWhile (== ' ') (reverse (take 5 (dropWhile (/= '1') line))))

convert :: Measurement -> Prefix -> Measurement
convert (MetricMeasurement x unit) prefix = MetricMeasurement (x * (read (parsePower (unitPrefix unit)) :: Double)  / (read (parsePower prefix) :: Double)) (M prefix (unitBase unit))

reportMeasurement :: Measurement -> String
reportMeasurement (MetricMeasurement x unit) 
    = show x ++ unitAbbr unit
