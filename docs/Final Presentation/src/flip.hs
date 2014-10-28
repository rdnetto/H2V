flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \a b -> f b a

--Using partial application to omit arguments
revsub :: Int -> Int -> Int
revsub = flip (-)

