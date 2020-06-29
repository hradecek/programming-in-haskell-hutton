-- type Move = Up | Down | Left | Right

-- type Pos = (Int, Int)

-- move :: Move -> Pos -> Pos
-- move Left (x, y) = Pos (x - 1, y)
-- move Right (x, y) = Pos (x + 1, y)
-- move Up (x, y) = Pos (x, y + 1)
-- move Down (x, y) = Pos (x, y - 1)

-- data Shape = Circle Float | Rect Float Float

-- square :: Float -> Shape
-- square x = Rect x x

-- area :: Shape -> Float
-- area (Circle r) = pi * r^2
-- area (Rect x y) = x * y

-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv m n = Just $ m `div` n

-- safehead :: [a] -> Maybe a
-- safehead [] = Nothing
-- safehead xs = Just $ head xs

data Nat = Zero | Succ Nat deriving Show

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree = Leaf Int | Node Tree Int Tree


