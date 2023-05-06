module Test.MySolutions where

import Data.Maybe
import Data.String
import Data.String.Regex
import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, Person, PhoneNumber, address, person)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.String.Common (joinWith)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V)
import Control.Monad.State.Class (state)
import Data.Traversable
import Control.Category (identity)
import Data.Semigroup (append)
import Data.Monoid (mempty)

-- Note to reader: Add your solutions to this file

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe:: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)

addApply :: forall f . Apply f => f Int -> f Int -> f Int
addApply = lift2 (+)

subApply:: forall f . Apply f => f Int -> f Int -> f Int
subApply = lift2 (-)

mulApply :: forall f . Apply f => f Int -> f Int -> f Int
mulApply = lift2 (*)

divApply :: forall f . Apply f => f Int -> f Int -> f Int
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just fa) = Just <$> fa
combineMaybe Nothing = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z\\s]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

nonEmptyImproved :: String -> String -> V Errors String
nonEmptyImproved field = matches field nonEmptyRegex

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- nonEmptyImproved "Street"  a.street
  city   <- nonEmptyImproved "City"    a.city
  state  <- matches "State" stateRegex a.state
  in address street city state


data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
    show Leaf = "Leaf"
    show (Branch l a r) = "(Branch " <> joinWith " " [show l, show a, show r] <> ")"


instance eqTree :: Eq a => Eq (Tree a) where
    eq Leaf Leaf = true
    eq _ Leaf = false
    eq Leaf _ = false
    eq (Branch l a r) (Branch ll aa rr) = eq a aa && eq l ll && eq r rr

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l a r) = Branch (map f l) (f a) (map f r)

instance foldableTree :: Foldable Tree where
    --foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldr _ zero Leaf = zero
    foldr f zero (Branch l a r) =
      let rr = foldr f zero r
          nzero = f a rr
      in foldr f nzero l
    --foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldl f zero Leaf = zero
    foldl f zero (Branch l a r) =
      let ll = foldl f zero l
          nzero = f ll a
      in foldl f nzero r
    --foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
    foldMap f = foldl (\b a -> b <> f a) mempty

instance traversableTree :: Traversable Tree where
    --traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
    traverse f Leaf = pure Leaf
    traverse f (Branch l a r) = ado
      ll <- traverse f l
      b <- f a
      rr <- traverse f r
      in Branch ll b rr
    --sequence :: forall a m. Applicative m => t (m a) -> m (t a)
    sequence xs = traverse identity xs

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder f Leaf = pure Leaf
traversePreOrder f (Branch l a r) = ado
      b <- f a
      ll <- traversePreOrder f l
      rr <- traversePreOrder f r
      in Branch ll b rr

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder f Leaf = pure Leaf
traversePostOrder f (Branch l a r) = ado
      ll <- traversePostOrder f l
      rr <- traversePostOrder f r
      b <- f a
      in Branch ll b rr

type Person2
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

person2 :: String -> String -> Maybe Address -> Array PhoneNumber -> Person2
person2 firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: Person2 -> V Errors Person2
validatePersonOptionalAddress p =
  person2 <$> nonEmpty "First Name" p.firstName
         <*> nonEmpty "Last Name" p.lastName
         <*> (sequence $ validateAddress <$> p.homeAddress)
         <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse:: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f = sequence <<< map f