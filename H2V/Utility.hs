module Utility where

import Data.List

-- Concatenates the map of a list of elements together, with a delimeter inserted between them.
joinMap delim f list = intercalate delim $ map f list

--applies function f to x N times
iterateN n f x = (iterate f x) !! n

