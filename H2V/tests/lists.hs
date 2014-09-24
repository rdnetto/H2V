import H2V_Compat

add2 :: [Int] -> [Int]
add2 xs = 1:2:xs

concatTest :: Int -> Int -> [Int]
concatTest a b = [a .. a + 3] ++ [b .. b + 3]

rangeTest :: Int -> Int -> [Int]
rangeTest m n = [m,m+2 .. n]

basicTest :: Int -> [Int]
basicTest x = [1, x, 2]

