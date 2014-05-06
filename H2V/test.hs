module Test (f) where

f x a b c = a2 * x + b * x + c where
    a2 = a * a

f1 x = x + d where
    a = 1
    b = 2
    c = d - a
    d = b + 2

-- Returns the nth fibonnacci number
-- Could be implemented using an infinite list, but that would be more painful to compile to Verilog
-- x0, x1 are accumulators, n counts down to the required index
fib n = fib' 0 1 n

fib' x0 _ 0 = x0
fib' x0 x1 n = fib' x1 (x0 + x1) (n - 1)
