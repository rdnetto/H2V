--higher order functions
--return a function
f6a :: Int -> (Int -> Int)
f6a a = \b -> a + b
f6 x = (f6a 1) x + (f6a 2) (x + 1)

