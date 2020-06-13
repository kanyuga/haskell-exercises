-- Database Processing
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1
justTheDates :: DatabaseItem -> [UTCTime] -> [UTCTime]
justTheDates (DbDate a) b = b ++ [a]
justTheDates _ b                      = b

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate x = foldr justTheDates [] x

-- 2
justTheInt :: DatabaseItem -> [Integer] -> [Integer]
justTheInt (DbNumber x) b = b ++ [x]
justTheInt _ b            = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber x = foldr justTheInt [] x

-- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent x = foldr max (UTCTime (fromGregorian 0 0 0) 0) (filterDbDate x)

-- 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum.filterDbNumber

-- 5
avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral (sumDb x) / fromIntegral (length (filterDbNumber x))


-- Scan Exercises
fibs = 1 : scanl (+) 1 fibs

-- 1
fibs20 = take 20 fibs

-- 2
fibsLT100 = takeWhile (<100) fibs

-- 3
-- scantorial = 1 : scanl (*) () scantorial

-- Chapter Exercises
-- - Warm-up and review
-- 1

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStop stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops ]

stopvowelstopStartsWithP :: [Char] -> [Char] -> [(Char, Char, Char)]
stopvowelstopStartsWithP stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p' ]

nounVerbNoun :: [String] -> [String] -> [(String, String, String)]
nounVerbNoun nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns ]

nounVerbNounStartsWithP :: [String] -> [String] -> [(String, String, String)]
nounVerbNounStartsWithP nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns, head x  == 'p']

-- 3
seekritFunc x =
  fromIntegral (sum (map length (words x))) /
  fromIntegral (length (words x))

-- Rewriting functions using folds
-- 1

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if (f a) == True then True else b) False

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a b -> if a == e then True else b) False
myElem' e = myAny (== e)

-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f(a) : b) []

-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f(a) then a : b else b) []

-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f(a) ++ b) []

-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)

-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x: xs) = foldr (\a b -> if f a b == GT then a else b) x (x: xs)

-- 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x: xs) = foldr (\a b -> if f a b == LT then a else b) x (x: xs)
