module Test (f) where

--due to a bug in haskell-src, this line is parsed incorrectly as (a2*x + b)*x + c without brackets
f x a b c = (a2 * x) + (b * x) + c where
    a2 = a * a

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

--higher order functions
f5 x = (flip (-)) 2 7 where
    flip f = \a -> \b -> f b a

-- Test case for nested, recursive functions
-- Returns the nth fibonnacci number
-- Could be implemented using an infinite list, but that would be more painful to compile to Verilog
-- x0, x1 are accumulators, n counts down to the required index
fib n = fib' 0 1 n where
    fib' x0 _ 0 = x0
    fib' x0 x1 n = fib' x1 (x0 + x1) (n - 1)
