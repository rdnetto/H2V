--This file enables compatility between H2V programs and normal Haskell compilers

module H2V_Compat where

--Parallelism operator - no-op on PCs
(|||) :: a -> Int -> a
(|||) x _ = x

--Monoid fold. Requires an associative folding function with a right-identity.
--Associative:   (a `f` b) `f` c == a `f` (b `f` c). Note that order is preserved.
--Right-identity: a `f` i == a
mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr = foldr

