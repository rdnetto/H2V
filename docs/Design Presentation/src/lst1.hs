xorArray :: Int -> [Int] -> [Int]
xorArray mask input = map (xor mask) input
