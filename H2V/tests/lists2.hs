import H2V_Compat

head :: [Int] -> Int
head (x0:xs) = x0
head [] = -1

tail :: [Int] -> [Int]
tail (x0:xs) = xs
tail [] = []

headTest a = head [1 .. 4]
tailTest a = tail [1 .. 4]

mapTest1 :: Int -> [Int]
mapTest1 a = map (+10) ([0 .. 16] ||| 4)

--this is equivalent to the case without any brackets around the call to map
mapTest2 :: Int -> [Int]
mapTest2 a = (map (+10) [0 .. 16]) ||| 4

