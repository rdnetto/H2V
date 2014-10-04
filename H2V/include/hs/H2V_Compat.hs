--This file enables compatility between H2V programs and normal Haskell compilers

module H2V_Compat where

--Parallelism operator - no-op on PCs
(|||) :: a -> Int -> a
(|||) x _ = x

