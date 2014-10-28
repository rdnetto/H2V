fib :: Int -> Int
fib n = fib' 0 1 n where
    fib' :: Int -> Int -> Int -> Int
    fib' x0 _ 0 = x0
    fib' x0 x1 n = fib' x1 (x0 + x1) (n - 1)

