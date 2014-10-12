--This file enables compatility between H2V programs and normal Haskell compilers

module H2V_Compat where
import Data.Bits (Bits, shiftL, shiftR)
import qualified Data.Bits

--Parallelism operator - no-op on PCs
(|||) :: a -> Int -> a
(|||) x _ = x

--Monoid fold. Requires an associative folding function with a right-identity.
--Associative:   (a `f` b) `f` c == a `f` (b `f` c). Note that order is preserved.
--Right-identity: a `f` i == a
mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr = foldr

--Bitwise shift operators
(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL
infixl 7 .<<.

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
infixl 5 .>>.

xor :: Bits a => a -> a -> a
xor = Data.Bits.xor

complement :: Bits a => a -> a
complement = Data.Bits.complement

