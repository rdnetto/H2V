import H2V_Compat

map :: (Int -> Int) -> [Int] -> [Int]
map f (x0:xs) = (f x0):(map f xs)

mapTest :: Int -> [Int]
mapTest a = map (+a) [0 .. 3] 

