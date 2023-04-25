module Test.MySolutions where

import Prelude
import Data.Boolean (otherwise)
import Data.Newtype
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array
import Data.Foldable as F

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