import H2V_Compat

sum :: [Int] -> Int
sum = mfoldr (+) 0 ||| 3

dotProduct :: [Int] -> [Int] -> Int
dotProduct u v = sum $ zipWith (*) u v ||| 3

demo _ = dotProduct [1, 2, 3] [4, 5, 6] ||| 3

