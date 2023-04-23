module Test.MySolutions where

import Data.Path
import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, cons, filter, find, foldl, head, last, mapMaybe, uncons, (..), (:))
import Data.Function (flip)
import Data.Int (rem)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst)
import Test.Examples (factors)
import Control.Alt ((<|>))
import Data.Profunctor.Strong ((&&&))
import Data.Foldable (fold)
import Data.Unfoldable as U


-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n = n `rem` 2 == 0

countEven :: Array Int -> Int
countEven arr =
  case uncons arr of
    Just {head: h, tail: t} ->
      let s = if (isEven h) then 1 else 0
      in s + countEven t
    Nothing                 -> 0

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infixl 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\x -> x >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = factors n == [[1, n]]


cartesianProduct :: forall a . Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

primeFactor :: Array Int -> Maybe (Tuple Int Int)
primeFactor [a, b] | isPrime a = Just $ Tuple a b
primeFactor [a, b] | isPrime b = Just $ Tuple b a
primeFactor _ = Nothing

primeFactors :: Int -> Array Int
primeFactors n =
  let xs = factors n
      pf :: Maybe (Tuple Int Int)
      pf = head $ mapMaybe primeFactor xs
  in maybe [] (\(Tuple a b) -> a : primeFactors b) pf

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec = go 0 1
  where
    go a b 0 = a
    go a b 1 = b
    go a b n = go b (a + b) (n - 1)

reverse :: forall a . Array a -> Array a
reverse = foldl (flip cons) []

onlyFiles :: Path -> Array Path
onlyFiles p | (not <<< isDirectory) p = [p]
onlyFiles p = concatMap onlyFiles (ls p)

filename' :: Path -> Maybe String
filename' = last <<< split (Pattern "/") <<< filename

onlyFiles' :: Path -> Array (Tuple Path Path)
onlyFiles' p = go p p
  where
    go d p | (not <<< isDirectory) p = [Tuple d p]
    go _ p = concatMap (go p) (ls p)

whereIs :: Path -> String -> Maybe Path
whereIs d name = map fst <<< find f $ onlyFiles' d
  where f (Tuple _ p) = filename' p == Just name

data LargestFile = NoLargestFile | LargestFile Path

instance Show LargestFile where
  show NoLargestFile = "Nothing"
  show (LargestFile path) = show path

instance Semigroup LargestFile where
  append NoLargestFile x = x
  append x NoLargestFile = x
  append x@(LargestFile (File _ sizeX)) y@(LargestFile (File _ sizeY)) | sizeX > sizeY = x
                                                                       | otherwise     = y
  append (LargestFile (Directory _ _)) (LargestFile (Directory _ _)) = NoLargestFile
  append (LargestFile (Directory _ _)) x = x
  append x (LargestFile (Directory _ _)) = x

instance Monoid LargestFile where
  mempty = NoLargestFile

newtype SmallestFile = SmallestFile (Maybe Path)

derive newtype instance showSmallestFile :: Show SmallestFile

instance Semigroup SmallestFile where
  append x@(SmallestFile (Just (File _ sizeX))) y@(SmallestFile (Just (File _ sizeY))) | sizeX < sizeY = x
                                                                                       | otherwise     = y
  append (SmallestFile x) (SmallestFile y) = SmallestFile (x <|> y)

instance Monoid SmallestFile where
  mempty = SmallestFile Nothing

smallestFile :: Path -> SmallestFile
smallestFile x@(File _ _) = SmallestFile (Just x)
smallestFile _ = SmallestFile Nothing

largestFile :: Path -> LargestFile
largestFile x@(File _ _ ) = LargestFile x
largestFile _ = NoLargestFile

largestSmallestFile :: Path -> Tuple LargestFile SmallestFile
largestSmallestFile = largestFile &&& smallestFile

class GetPath a where
  getPath :: a -> Maybe Path

instance GetPath LargestFile where
  getPath NoLargestFile   = Nothing
  getPath (LargestFile p) = Just p

instance GetPath SmallestFile where
  getPath (SmallestFile p) = p

largestSmallest :: Path -> Array Path
largestSmallest p = go (fold $ largestSmallestFile <$> onlyFiles p)
  where
    notSamePath :: forall a b . GetPath a => GetPath b => a -> b -> Boolean
    notSamePath x y = show (getPath x) /= show (getPath y)
    go (Tuple x y) | notSamePath x y = U.fromMaybe (getPath x) <> U.fromMaybe (getPath y)
    go (Tuple x y)                   = U.fromMaybe (getPath x)
