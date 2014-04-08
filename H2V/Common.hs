module Common where

import Data.List
import Language.Haskell.Pretty (Pretty, prettyPrint)
import Language.Haskell.Syntax

-- Concatenates the map of a list of elements together, with a delimeter inserted between them.
joinMap delim f list = intercalate delim $ map f list

--applies function f to x N times
iterateN n f x = (iterate f x) !! n

--helper function used to display parsing errors
pshow :: Show a => Pretty a => a -> String
pshow x = "\n" ++ show x ++ "\n" ++ prettyPrint x

--DFD coreFuncs allFunctions
--coreFuncs contains the names of functions which are exported
--allFunctions is a list of all the functions defined in the file root of the file
data DFD_old = DFD_old [HsName] [Function] deriving Show

--Function(name, arguments, expression)
--arguments is a list of name-type tuples (names will be generated if none are present)
data Function = Function HsName [HsName] HsExp deriving Show

