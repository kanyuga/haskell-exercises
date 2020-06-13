-- 1
data TisAnInteger = 
  TisAn Integer

instance Eq TisAnInteger where
  (TisAn i') == (TisAn i) = i == i'

-- 2
data TwoIntegers = 
  Two Integer Integer

instance Eq TwoIntegers where
  (Two i j) == (Two i' j') = i == i' && j == j'

-- 3
data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (TisAnInt i) == (TisAnInt i') = i == i'
  (TisAString i) == (TisAString i') = i == i'
  _ == _ = False

-- 4
data Pair a = 
  Pair a a

instance Eq a => Eq (Pair a) where
  (Pair a a'') == (Pair a' a''') = a == a' && a''' == a''

-- 5
data Tuple a b = 
  Tuple a b

instance (Eq x, Eq y) => Eq (Tuple x y) where
  (Tuple a b) == (Tuple a' b') = a == a' && b == b'

-- 6
data Which a = 
    ThisOne a 
  | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a) == (ThisOne a') = a == a'
  (ThatOne a) == (ThatOne a') = a == a'
  _ == _ = False

-- 7
data EitherOr a b = 
    Hello a 
  | Goodbye b

instance (Eq a, Eq b) => Eq(EitherOr a b) where
  (Hello a) == (Hello a') = a == a'
  (Goodbye b) == (Goodbye b') = b == b'
  _ == _ = False
