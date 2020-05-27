-- * 3.11 Exercises
-- ** 1.
-- | What are the types of the following values?
['a', 'b', 'c']             :: [Char]
('a', 'b', 'c')             :: (Char, Char, Char)
[(False, 'O'), (True, '1')] :: [(Bool, Char)]
([False, True], ['0', '1']) :: ([Bool], [Char])
[tail, init, reverse]       :: [[a] -> [a]]

-- ** 2.
-- | What are types of the following functions?
second xs = head (tail xs)       :: [a] -> a
swap (x, y) = (y, x)             :: (a, b) -> (b, a)
pair x y = (x, y)                :: x -> y -> (x, y)
double x = x * 2                 :: Num a => a -> a
palindrome xs = reverse xs == xs :: Eq a => [a] -> Bool
twice f x = f (f x)              :: (a -> a) -> a -> a

-- ** 4.
-- | Why is it not feasible in general for function types to be instances of the Eq
-- class? When is it feasible? Hint: two functions of the same type are equal if
-- they always return equal results for equal arguments.
-- Solution:
-- Functions f1 and f2 are equals only if and only if all their possible arguments produce same result.
-- Enumerating all results is possible only for functions, which inputs are from finite set e.g. Bool.
