import H2V_Compat

add2 :: [Int] -> [Int]
add2 xs = 1:2:xs

concat :: [Int] -> [Int]  -> [Int] 
concat a b = a ++ b

rangeTest :: Int -> Int -> [Int]
rangeTest m n = [m .. n]

basicTest :: Int -> [Int]
basicTest x = [1, x, 2]

