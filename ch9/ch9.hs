import Data.Char

-- EnumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool start stop
  | start < stop = [start, stop]
  | stop > start = []
  | otherwise    = [start]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop = go start []
  where go start' orderedList
         | start' < stop  = go (succ start') (orderedList ++ [start'])
         | start' == stop = orderedList ++ [start']
         | otherwise     = []

eftInt :: Int -> Int -> [Int]
eftInt start stop = go start []
  where go start' orderedList
         | start' < stop  = go (succ start') (orderedList ++ [start'])
         | start' == stop = orderedList ++ [start']
         | otherwise     = []
         
eftChar :: Char -> Char -> [Char]
eftChar start stop = go start []
  where go start' orderedList
         | start' < stop  = go (succ start') (orderedList ++ [start'])
         | start' == stop = orderedList ++ [start']
         | otherwise     = []


-- Thy Fearful Symmetry
-- 1
myWords :: String -> [String]
myWords str = go str []
  where go s stringList
         | length s == 0 = stringList
         | otherwise     = stringList ++
                             [takeWhile isNotSpace s] ++
                             go (drop 1 $ dropWhile isNotSpace s) stringList
        isNotSpace = (/= ' ')

-- 2
myLines :: String -> [String]
myLines str = go str []
  where go s stringList
         | length s == 0 = stringList
         | otherwise     = stringList ++
                             [takeWhile isNotNewLine s] ++
                             go (drop 1 $ dropWhile isNotNewLine s) stringList
        isNotNewLine = (/= '\n')

--3

tokenizer :: String -> Char -> [String]
tokenizer str token = go str []
 where go s stringList
        | length s == 0 = stringList
        | otherwise     = stringList ++
                             [takeWhile isNotToken s] ++
                             go (drop 1 $ dropWhile isNotToken s) stringList
       isNotToken = (/= token)

-- Square Cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1, 2
myTuple = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3
tupleLength = length myTuple

-- Filtering
-- 1
multiplesOf3 = filter (\x -> (x `rem` 3) == 0) [1..30]

-- 2
lenFilter = length . filter (\x -> (x `rem` 3) == 0)

-- 3
articleFilter :: String -> [String]
articleFilter str = filter (\x -> not $ x `elem` ["the", "a", "an"]) (words str)

-- Zipping

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a: a') (b: b') = [(a, b)] ++ myZip a' b'

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f _ [] = [];
myZipWith f [] _ = [];
myZipWith f (a: a') (b: b') = [f a b] ++ myZipWith f a' b'

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 a b = myZipWith (\x -> \y -> (x, y)) a b

-- Data.Char
-- 2
upperCaseOnly :: String -> String
upperCaseOnly a = filter isUpper a

-- 3
capitalize :: String -> String
capitalize (a:xs) = [toUpper a] ++ xs

-- 4
toAllUpper :: String -> String
toAllUpper [] = ""
toAllUpper (a:xs) = [toUpper a] ++ toAllUpper xs

-- 5,6
capitalizedHead :: String -> Char
capitalizedHead = toUpper.head

-- Ciphers

caesarCipher :: String -> Int -> String
caesarCipher "" _ = ""
caesarCipher (x: xs) offset = [chrWrap (ord x + offset)] ++ caesarCipher xs offset
  where ordA = ord 'a'
        chrWrap a = chr $ ordA + (a - ordA) `mod` 26

caesarUncipher :: String -> Int -> String
caesarUncipher "" _ = ""
caesarUncipher (x: xs) offset = [chrWrap (ord x - offset)] ++ caesarUncipher xs offset
  where ordA = ord 'a'
        chrWrap a = chr $ ordA + (a - ordA) `mod` 26

-- Writing your own standard functions
-- 1
myOr :: [Bool] -> Bool
myOr [] = True
myOr (x: xs) = x || myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x: xs) = f x || myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x: xs) = a == x || myElem a xs

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a l = myAny (\x -> x == a) l

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x: xs) = myReverse xs ++ [x]

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish (x: xs) = x ++ squish xs

-- 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x: xs) = f x ++ squishMap f xs

-- 7
squishAgain :: [[a]] -> [a]
squishAgain a = squishMap (\x -> x) a

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [a]           = a
myMaximumBy maxFunc (x: xs) = go xs x
  where go (first: rest) currentMax
         | length rest == 0 = currentMax
         | otherwise        = case (maxFunc first currentMax) of
                                LT -> go rest currentMax
                                _  -> go rest first
                                
-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [a]           = a
myMinimumBy minFunc (x: xs) = go xs x
  where go (first: rest) currentMin
         | length rest == 0 = currentMin
         | otherwise        = case (minFunc first currentMin) of
                                GT -> go rest currentMin
                                _  -> go rest first


myMaximum :: (Ord a) => [a] -> a
myMaximum a = myMaximumBy compare a

myMinimum :: (Ord a) => [a] -> a
myMinimum a = myMinimumBy compare a
