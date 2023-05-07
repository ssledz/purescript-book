module Test.MySolutions where

import Data.Foldable
import Prelude

import Control.Monad.ST (ST, for, run)
import Control.Monad.ST.Ref (modify, new, read)
import Data.Array (head, nub, sort, tail)
import Data.Int (pow, toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Exception.Unsafe (unsafeThrow, unsafeThrowException)
import Debug

-- Note to reader: Add your solutions to this file

third :: forall a . Array a -> Maybe a
third xs = tail xs >>= tail >>= head

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< nub <<< foldM f 0
  where
    f :: Int -> Int -> Array Int
    f acc a = [acc, acc + a]

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x:xs) = f x >>= \b ->
  if (b) then Cons x <$> filterM f xs else filterM f xs


exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure (a / b)

estimatePi :: Int -> Number
estimatePi n = 4.0 * run do
  accRef <- new 0.0
  for 1 (n + 1) \k ->
    flip modify accRef \acc ->
      --trace ("acc: " <> show acc) \_ ->
        acc + (toNumber $ pow (-1) (k + 1)) / (2.0 * toNumber k - 1.0)
  read accRef


