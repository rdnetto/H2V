module Test where

--due to a bug in haskell-src, this line is parsed incorrectly as (a2*x + b)*x + c without brackets
f x a b c = (a2 * x) + (b * x) + c where
    a2 = a * a

f1 :: Int -> Int
f1 x = x + c where
    a = 1
    b = 2
    c = d - a
    d = b + 2

--pattern matching
f3 0 = 0
f3 x = if x < 0
       then -1
       else 1

--pattern guards
f4 x
    | x < 0     = 0
    | x > 0     = 1
    | otherwise = 2

fLambda _ = (\a b -> a + b) 1 2

--higher order functions
--return a function
f6a :: Int -> (Int -> Int)
f6a a = \b -> a + b
f6 x = (f6a 1) x + (f6a 2) (x + 1)

--partial application
plus x y = x + y
add1 = plus 1
add2 = (+2)
add3 = (3+)
f7 x = (add1 x) + (add2 x) + (add3 x)

--accept a function
applyTwice :: (Int -> Int) -> (Int -> Int)
applyTwice f = \x -> f (f x)
f8 x = applyTwice (\x -> x + 1) 0

--both
f5 x = x + (revsub 2 7) + (revsub 9 10) where
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \a b -> f b a
revsub = flip (-)

-- Test case for nested, recursive functions
-- Returns the nth fibonnacci number
-- Could be implemented using an infinite list, but that would be more painful to compile to Verilog
-- x0, x1 are accumulators, n counts down to the required index
fib n = fib' 0 1 n where
    fib' x0 _ 0 = x0
    fib' x0 x1 n = fib' x1 (x0 + x1) (n - 1)
