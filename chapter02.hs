-- * 2.6 Exercises
-- ** 1.
-- | Parenthesise the following arithmetic expressions:
-- @
--     2 ^ 3 * 4 == (2 ^ 3) * 4
--     2 * 3 + 4 * 5 == (2 * 3) + (4 * 5)
--     2 + 3 * 4 ^ 5 == 2 + (3 * (4 ^ 5))
-- @

-- ** 3.
-- | The script below contains three syntactic errors. Correct these errors and then
-- check that your script works properly using Hugs.
-- Solution:
-- - function names MUST be lowercased
-- - function using infix notation MUS be enclosed by backitcks `div`
-- - evaluation a `div` length does not make sense, MUST be parenthesed (length xs)
n :: Integer
n = a `div` (fromIntegral (length xs))
     where a = 10 :: Integer
           xs = [1,2,3,4,5] :: [Int]

-- ** 4.
-- | Show how the library function last that selects the last element of a nonempty
-- list could be defined in terms of the library functions introduced in
-- this chapter. Can you think of another possible definition?
last1, last2, last3, last4 :: [a] -> a
last1 x = head $ drop ((length x) - 1) x

last2 x  = x !! (length x - 1)

last3 []  = undefined
last3 [x] = x
last3 (_:xs) = last3 xs

last4 [x] = x
last4 xs  = last4 $ tail xs
