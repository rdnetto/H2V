import H2V_Compat

concatTest :: Int -> Int -> [Int]
concatTest a b = [a .. a + 3] ++ [b .. b + 3]

concatTest2 :: Int -> [Int]
concatTest2 a  = [0, 1] ++ [2, 3] ++ [4, 5]

rangeTest :: Int -> Int -> [Int]
rangeTest m n = [m,m+2 .. n]

basicTest :: Int -> [Int]
basicTest x = [1, x, 2]

consTest1 :: Int -> [Int]
consTest1 a = a:[3, 4, 5]

consTest2 :: Int -> [Int]
consTest2 a = a:[]

consTest3 :: Int -> Int -> [Int]
consTest3 a b = a:b:[3, 4, 5]

