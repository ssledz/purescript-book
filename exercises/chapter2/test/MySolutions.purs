module Test.MySolutions where

import Prelude
import Data.Number (pi, sqrt)
import Data.Int (rem)

diagonal x y = sqrt (x * x + y * y)

circleArea r = pi * r * r

leftoverCents c = c `rem` 100
