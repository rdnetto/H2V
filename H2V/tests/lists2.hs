import H2V_Compat

head :: [Int] -> Int
head (x0:xs) = x0
head [] = -1

tail :: [Int] -> [Int]
tail (x0:xs) = xs
tail [] = []

headTest a = head [1 .. 4]
tailTest a = tail [1 .. 4]


