module Test.MySolutions where

import Prelude
import Math (sqrt, pi, e)
import Global

diagonal w h = sqrt (w * w + h * h)

circleArea radis = radis * radis * pi

--  readFloat :: String -> Number

addE s = e + readFloat s
