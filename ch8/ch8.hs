module Ch8 where
import Data.List (intercalate)

-- Recursion
-- 2
summer :: (Eq a, Num a) => a -> a
summer 1 = 1
summer n = n + summer(n - 1)

-- 3
multiplier :: (Integral a) => a -> a -> a
multiplier a 1 = a
multiplier a n = a + multiplier a (n-1)

-- Fixing DividedBy
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | (abs n) < (abs d) = (count, n)
          | otherwise         = go ((abs n) - (abs d)) d (inc count)
        abs n
          | n < 0     = negate n
          | otherwise = n
        inc c
          | num >= 0 && denom >= 0 = c + 1
          | num < 0 && denom < 0   = c + 1
          | otherwise              = c - 1


-- McCarthy 91
mc91 :: Integer -> Integer
mc91 n
  | n < 100   = 91
  | otherwise = n -10

-- Numbers into words

digitToWord :: Int -> String
digitToWord n = digitNames !! n
  where digitNames = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]

digits :: Int -> [Int]
digits num = go num []
  where go n digitList
          | n < 10    = [n] ++ digitList
          | otherwise = go (n `div` 10) ([n `mod` 10] ++ digitList)

wordNumber :: Int -> String
wordNumber n = intercalate "-" digitsToWords
  where digitsToWords = map digitToWord (digits n)
