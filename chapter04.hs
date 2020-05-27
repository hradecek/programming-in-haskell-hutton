-- * 4.8 Execrices
-- ** 1.
-- | Using library functions, define a function halve :: [a] -> ([a], [a]) that
-- splits an even-lengthed list into two halves. For example:
halve :: [a] -> ([a], [a])
halve x = (left, right)
            where left = take half x
                  right = drop half x
                  half = (length x) `div` 2

-- ** 2.
-- | Consider a function @safetail :: [a] -> [a]@ that behaves as the library
-- function @tail@, except that @safetail@ maps the empty list to itself, whereas @tail@
-- produces an error in this case. Define safetail using:
-- a ~> a conditional expression;
-- b ~> guarded equations;
-- c ~> pattern matching.
safetail1 :: [a] -> [a]
safetail1 x = if (length x) == 0 then x else tail x

safetail2 :: [a] -> [a]
safetail2 x | (length x) == 0 = x
            | otherwise       = tail x

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 x = tail x

-- ** 3.
-- | In a similar way to ∧, show how the logical disjunction operator ∨ can be
-- defined in four different ways using pattern matching
(|||) :: Bool -> Bool -> Bool
False ||| False = False
False ||| True  = True
True  ||| False = True
True  ||| True  = True

-- ** 4.
-- | Redefine the following version of the conjunction operator using conditional
-- expressions rather than pattern matching:
-- @
--     True ∧ True = True
--     _    ∧  _   = False
-- @
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True then
            if b == True then True else False
          else False

-- ** 5.
-- | Do the same for the following version, and note the difference in the number
-- of conditional expressions required:

-- @
--     True ∧ b  = b
--     False ∧ _ = False
-- @
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else False

-- ** 6.
-- | Show how the curried function definition @mult x y z = x ∗ y ∗ z@ can be
-- understood in terms of lambda expressions.
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z
