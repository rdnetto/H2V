--accept a function
applyTwice :: (Int -> Int) -> (Int -> Int)
applyTwice f = \x -> f (f x)
f8 x = applyTwice (\x -> x + 1) 0

