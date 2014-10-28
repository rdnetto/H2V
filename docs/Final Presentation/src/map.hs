xorArray :: Int -> [Int] -> [Int]
xorArray mask input = map (xor mask) input

map :: (a -> b) -> [a] -> [b]       --built-in
xor :: Int -> Int -> Int            --built-in
