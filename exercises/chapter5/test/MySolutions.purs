module Test.MySolutions where

import Prelude

import Data.Person (Person)
import Data.Picture (Point, Shape(..), origin)
import Data.Maybe (Maybe(..))
import ChapterExamples (Amp(..), Volt(..))
import Data.Number (pi)

factorial' :: Int -> Int
factorial' 0 = 1
factorial' 1 = 1
factorial' n = n * factorial' (n - 1)

factorial :: Int -> Int
factorial = go 1
  where
    go acc 0 = acc
    go acc 1 = acc
    go acc n = go (n * acc) (n - 1)

binomial :: Int -> Int -> Int
binomial n k | n == k = 1
binomial n k | n < k  = 0
binomial n k  = factorial n / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal n k | n < k   = 0
pascal 0 _  = 1
pascal _ 0   = 1
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address : { city: x }} { address : { city: y }} = x == y

type Person' = forall r s . { address :: { city :: String | r} | s}

sameCity' :: Person' -> Person' -> Boolean
sameCity' { address : { city: x }} { address : { city: y }} = x == y

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton d _   = d

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

double :: Number -> Number
double x = x * 2.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (double r)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (double w) (double h)
doubleScaleAndCenter (Line {x : sx, y : sy} {x : ex, y : ey}) =
  let w = ex - sx
      h = ey - sy
  in Line { x: -w, y: -h } { x: w, y: h }
doubleScaleAndCenter (Text _ txt) = Text origin txt
doubleScaleAndCenter (Clipped p _ w h) = Clipped p origin (double w) (double h)

shapeText :: Shape -> Maybe String
shapeText (Text _ txt) = Just txt
shapeText _            = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt u) = Watt (i * u)

area :: Shape -> Number
area (Line _ _) = 0.0
area (Text _ _) = 0.0
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Clipped _ _ w h) = w * h