import H2V_Compat

sum :: [Int] -> Int
sum = mfoldr (+) 0

dotProduct :: [Int] -> [Int] -> Int
dotProduct = sum . zipWith (*)

demo _ = dotProduct [1, 2, 3] [4, 5, 6] ||| 3

