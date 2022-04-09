module Isogram where

import Data.Char
import Data.List

isIsogram :: String -> Bool
isIsogram str = duplSearch $ sort $ map toLower str

duplSearch :: String -> Bool
duplSearch [] = True
duplSearch [x] = True
duplSearch (x : y : tail) = x /= y && duplSearch (y : tail)

isIsogram' :: String -> Bool
isIsogram' str = null $ map toLower str \\ ['a'..'z']
