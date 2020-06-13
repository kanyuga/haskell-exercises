-- Grab Bag
-- 3 a
addOneIfOdd n = case odd n of
  True -> (\n -> n + 1) n
  False -> n

-- 3 b
addFive = \x -> \y -> (if x > y then y else x) + 5

-- 3 c
mflip f x y = f y x


-- Variety Pack
-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))


-- Case Practice
-- 1
functionC x y =
  case (x > y) of
    True -> x
    False -> y

-- 2
ifEvenAdd2 n =
  case even n of
   True -> n + 2
   False -> n

-- 3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


-- End of Chapter Exercises
-- 1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst $ x `divMod` 10
        d     = snd $ xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = fst $ x `divMod` 100
        d     = snd $ xLast `divMod` 10

-- 2

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True -> x
  False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
 | b == True = x
 | otherwise = y

-- 3

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f(a), c)

-- 5
roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

-- 6
module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip 4 :: Integer)
  print (id 4)
