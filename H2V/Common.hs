module Common where

import Control.Monad.State
import Data.Char (isSpace)
import Data.Either
import Data.List
import Debug.Trace
import Language.Haskell.Pretty (Pretty, prettyPrint)
import Language.Haskell.Syntax

-- Concatenates the map of a list of elements together, with a delimeter inserted between them.
joinMap :: [a] -> (b -> [a]) -> [b] -> [a]
joinMap delim f list = intercalate delim $ map f list

--like concatMap, but for monads
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = do
    ys <- mapM f xs
    return $ concat ys

--applies function f to x N times
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x = (iterate f x) !! n

--returns a list without its first N elements
headlessN :: Int -> [a] -> [a]
headlessN 0 xs = xs
headlessN n (x0:xs) = headlessN (n - 1) xs

--Replace all substrings s1 of s0 with s2
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace s1 s2 s0 = res where
    res = if isPrefixOf s1 s0
        then replace s1 s2 (s2 ++ (headlessN (length s1) s0))
        else (head s0):(replace s1 s2 (tail s0))

--helper function used to display parsing errors
pshow :: Show a => Pretty a => a -> String
pshow x = "\n" ++ show x ++ "\n" ++ prettyPrint x

--helper function for logging values
tShow msg x = tShowX show msg x
tShowX f msg x = trace (msg ++ ": " ++ (f x)) x

--helper function for mapping over tuples
map2 :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
map2 f1 f2 = \(x, y) -> (f1 x, f2 y)

--Converts a list of tuples to a tuple of lists
splitTuple :: [(a, b)] -> ([a], [b])
splitTuple ts = (as, bs) where
    as = map fst ts
    bs = map snd ts

--The documentation says these should be in Data.Either, but GHC can't find it...
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

both :: [Either a a] -> [a]
both = map f where
    f (Left a)  = a
    f (Right a) = a

--Convenience function for removing trailing commas
chopComma :: String -> String
chopComma s = reverse (a ++ b) where
    (a, _:b) = span (/= ',') $ reverse s

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

--Applies the mapping function to all elements satisfying the predicate
filterMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterMap predicate f xs = map f' xs where
    f' x
        | predicate x = f x
        | otherwise   = x

headOr :: a -> [a] -> a
headOr _ (x:_) = x
headOr x [] = x

