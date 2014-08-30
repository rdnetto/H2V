flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \a b -> f b a

revsub :: Int -> Int
revsub = flip (-)

f5 x = x + (revsub 2 7) + (revsub 9 10) where
