module Test.MySolutions where
import Prelude
import Data.Array(concat, head, tail, filter, (:),(..))
import Data.Maybe(fromMaybe)
import Test.Examples (factors)
import Control.MonadZero (guard)
import Data.Int (rem, quot)
import Data.Foldable
import Data.Boolean
-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not isEven (n - 1)

countEven :: Array Int -> Int
countEven [] = 0
countEven lt = countIfEven (fromMaybe 0 $ head lt) + (countEven $ fromMaybe [] $ tail lt) 
    where
    countIfEven :: Int -> Int
    countIfEven lt' = case isEven lt' of
                          true -> 1
                          false -> 0

squared :: Array Number -> Array Number
squared arr = map (\n -> n * n) arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (\n -> n >=  0.0) arr

infix 4 filter as <$?>
keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite lt = (\n -> n >= 0.0) <$?> lt

isPrime :: Int -> Boolean
isPrime t = t > 1 && length (factors t) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct lt1 lt2 = do
    i <- lt1
    j <- lt2
    pure [ i, j ]

triples :: Int -> Array (Array Int)
triples n = do
    a <- 1 .. n
    b <- a  .. n
    c <- b  .. n
    guard $ a * a + b * b == c * c
    pure [ a, b, c ]

factorize :: Int -> Array Int
factorize n = factorize' 2 n []
    where
    factorize' :: Int -> Int -> Array Int -> Array Int
    factorize' _ 1 result = result
    factorize' divisor dividend result =
        let
            remainder = rem dividend divisor
        in
            if remainder == 0 then
                factorize' (divisor) (quot dividend divisor)  (divisor : result)
            else
                factorize' (divisor + 1) dividend result

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\b it -> b && it) true

fibTailRec :: Int -> Int
fibTailRec n = fibTailRec' n 0 0 1
    where 
          fibTailRec' :: Int -> Int -> Int -> Int -> Int
          fibTailRec' l c n1 n2 =
              if l == c 
                  then n1 + n2
                  else fibTailRec' l (c + 1) (n1 + n2) n1
                  
reverse :: forall a. Array a -> Array a
reverse = foldl (\a it -> it : a ) []

