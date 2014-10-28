main :: Int -> Int
main a = x + z where
    x = foo a
    y = bar a            --can be optimized out
    z = baz a

