module Test.MySolutions where

import Data.Array
import Data.Int
import Data.Maybe
import Data.Newtype
import Prelude

import Data.Boolean (otherwise)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable as F
import Data.String.Common (joinWith)
import Data.Function (on)
import Data.Char (toCharCode)
import Data.String.CodeUnits (toCharArray)

-- Note to reader: Add your solutions to this file

newtype Point = Point { x:: Number, y:: Number }

instance showPoint :: Show Point where
  show (Point p) = "(" <> show p.x <> ", " <> show p.y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex c) = show c.real <> sign c.imaginary <> show c.imaginary <> "i"
    where
      sign n | n < 0.0 = ""
             | otherwise = "+"

derive newtype instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

op f a b = {real: a.real `f` b.real, imaginary: a.imaginary `f` b.imaginary}

instance semiringComplex :: Semiring Complex where
  add = over2 Complex (op (+))
  zero = wrap { real: 0.0, imaginary: 0.0 }
  mul = over2 Complex $ \x y ->
        { real     : x.real * y.real - x.imaginary * y.imaginary
        , imaginary: x.imaginary * y.real + x.real * y.imaginary
        }
  one = wrap { real: 1.0, imaginary: 0.0 }

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance shapeGeneric :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a arr) (NonEmpty a' arr') = eq a a' && eq arr arr'

derive instance nonEmptyGeneric :: Generic (NonEmpty a) _

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show = genericShow

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a arr) (NonEmpty a' arr') = NonEmpty a (arr `append` (a' : arr'))

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a arr) = NonEmpty (f a) (f <$> arr)

data Extended a = Infinite | Finite a

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite      = true
  eq (Finite a) (Finite a') = a == a'
  eq _ _                    = false


instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite      = EQ
  compare Infinite a             = GT
  compare a Infinite             = LT
  compare (Finite a) (Finite a') = compare a a'

instance foldableNonEmpty :: F.Foldable NonEmpty where
  foldr f zero (NonEmpty a arr) = foldr f zero $ a:arr
  foldl f zero (NonEmpty a arr) = foldl f zero $ a:arr
  foldMap f (NonEmpty a arr) = foldMap f $ a:arr

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: F.Foldable f => F.Foldable (OneMore f) where
  foldr f zero (OneMore a fa) = f a (F.foldr f zero fa)
  foldl f zero (OneMore a fa) = F.foldl f (f zero a) fa
  foldMap f (OneMore a fa) = f a `append` F.foldMap f fa

derive instance eqShape :: Eq Shape
derive instance eqPoint :: Eq Point

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordShape :: Ord Shape
derive instance ordPoint :: Ord Point

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< F.maximum

newtype Multiply = Multiply Int

derive newtype instance showMultiply :: Show Multiply
derive newtype instance eqMultiply :: Eq Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

derive instance newtypeMultiplt :: Newtype Multiply _

class Monoid m <= Action m a where
  act :: m -> a -> a

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) a = n * a
--  act (Multiply n) a = pow a n

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) = joinWith "" <<< replicate n

instance actionArr :: Action m a => Action m (Array a) where
  act m = map (act m)

newtype Self m = Self m

derive newtype instance showSelf :: Show m => Show (Self m)
derive newtype instance eqSelf :: Eq m => Eq (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m (Self n) = Self (m <> n)

newtype HashCode = HashCode Int

derive newtype instance hashShow :: Show HashCode

instance hashCodeEq :: Eq HashCode where
  eq (HashCode a) (HashCode b) = a == b

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

arrayHasDuplicates :: forall a . Hashable a => Array a -> Boolean
arrayHasDuplicates arr =
  let arr' = nubByEq (\a b -> hashEqual a b && a == b) arr
  in length arr' /= length arr

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour n) = hash (mod n 12)