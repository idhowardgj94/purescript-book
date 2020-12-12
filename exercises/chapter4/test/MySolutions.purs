module Test.MySolutions where

import Prelude
import Data.Array(head, tail)
import Data.Maybe(fromMaybe)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not isEven (n - 1)

countEven :: Array Int -> Int
countEven [] = 0
countEven lt = countIfEven (fromMaybe 0 $ head lt) + (countEven $ fromMaybe [] $ tail lt) 
    where
    countIfEven :: Int -> Int
    countIfEven lt = case isEven lt of
                          true -> 1
                          false -> 0
