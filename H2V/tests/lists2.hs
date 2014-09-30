import H2V_Compat

head :: [Int] -> Int
head (x0:xs) = x0
head [] = -1

headTest a = head [1 .. 4]

