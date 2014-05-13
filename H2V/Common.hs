module Common where

import Control.Monad.State
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
tShow msg x = trace (msg ++ ": " ++ show x) x

--DFD coreFuncs allFunctions
--coreFuncs contains the names of functions which are exported
--allFunctions is a list of all the functions defined in the file root of the file
data DFD_old = DFD_old [HsName] [Function] deriving Show

--Function(name, arguments, expression)
--arguments is a list of name-type tuples (names will be generated if none are present)
data Function = Function HsName [HsName] HsExp deriving Show

