module Data.Operator.Generic.Rep
  ( class Generic1
  , from1
  , to1
  ) where

import Prelude (Unit, unit, ($))

import Data.Functor (class Functor, map)
import Data.Generic.Rep (class Generic, from, to)

-- | The `Generic1` class asserts the existence of a function from types
-- | of kind `Type -> Type` to their representations using the type
-- | constructors defined in the `Data.Generic.Rep` module of the
-- | 'generics-rep' package.
class Generic1 (f :: Type -> Type) rep | f -> rep where
  from1 :: forall a. f a -> rep
  to1 :: rep -> f Unit

instance generic1Generic
  :: ( Functor f
     , Generic (f Unit) rep
     )
  => Generic1 f rep
  where
  from1 x = from $ map (\_ -> unit) x
  to1 = to
