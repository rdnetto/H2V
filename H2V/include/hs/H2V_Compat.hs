--This file enables compatility between H2V programs and normal Haskell compilers

pmap :: Int -> (a -> b) -> [a] -> [b]
pmap _ = map
