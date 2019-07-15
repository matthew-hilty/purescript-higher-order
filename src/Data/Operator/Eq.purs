module Data.Operator.Eq
  ( class Eq1
  , class GenericEq1
  , eq1        , (.==)
  , genericEq1
  , notEq1     , (./=)
  ) where

import Data.Either (Either(Left, Right))
import Data.EitherR (EitherR(EitherR))
import Data.Eq (class Eq, eq)
import Data.Generic.Rep (Constructor, Product(Product), Sum(Inl, Inr))
import Data.List (List(Nil))
import Data.Operator.Generic.Rep (class Generic1, from1)

-- | The `Eq1` typeclass of this module represents type constructors `f` that
-- | have an associated equivalence relation of values of type `f a`
-- | independent of any choice of `a`.
-- |
-- | Unlike the `Data.Eq.Eq1` typeclass of the 'Prelude' module [hereafter,
-- | qualified by the prefix "Prelude"], `Eq1`, defined here, is not generally
-- | equivalent to the `Eq` typeclass (also found in the 'Prelude' module).
-- |
-- | For example, whereas both
-- |
-- |  `eq (Cons 0 Nil) (Cons 1 Nil) == false`
-- |
-- | and
-- |
-- |  `Prelude.eq1 (Cons 0 Nil) (Cons 1 Nil) == false`
-- |
-- | the definition found in this module gives the following:
-- |
-- |  `eq1 (Cons 0 Nil) (Cons 1 Nil) == true`
-- |
-- | That is, the `Int` values in a value of `List Int` (and, more generally,
-- | the `a` values in any `List a`) have little relevance in determining the
-- | results of the `eq1` function.
-- |
-- | The instances of the `Eq1` typeclass of this module are designed to
-- | emphasize the value-level effects of the type constructors themselves.
-- | In practice, this often means that an equivalence relation is assigned to
-- | the type constructor's data constructors. The `Eq1` instance declaration
-- | for `List`, for example, effectively projects any value of type `List a`
-- | into one of two equivalence classes, where the specific equivalence-class
-- | assignment is determined by the data constructor (`Cons` or `Nil`) of the
-- | value's head deconstruction.
-- |
-- | This interpretation of `Eq1` exists in addition to `Prelude.Eq1`
-- | because datatypes often have intrinsic (or designed) subgroupings,
-- | commonly distingished by data constructors, and a means to relate these
-- | different subgroupings can be useful -- especially in concert with
-- | `PartialOrd1` and other related typeclasses.
-- |
class Eq1 f where
  eq1 :: forall a. f a -> f a -> Boolean

infixl 4 eq1 as .==

notEq1 :: forall a f. Eq1 f => f a -> f a -> Boolean
notEq1 x y = (x `eq1` y) `eq` false

infix 4 notEq1 as ./=

instance eq1Array :: Eq1 Array where
  eq1 [] [] = true
  eq1 [] _  = false
  eq1 _  [] = false
  eq1 _  _  = true

else instance eq1Either :: Eq a => Eq1 (Either a) where
  eq1 (Left x)  (Left y)  = x `eq` y
  eq1 (Right _) (Right _) = true
  eq1 _         _         = false

else instance eq1EitherR :: Eq a => Eq1 (EitherR a) where
  eq1 (EitherR (Left _))  (EitherR (Left _))  = true
  eq1 (EitherR (Right x)) (EitherR (Right y)) = x `eq` y
  eq1 _                   _                   = false

else instance eq1List :: Eq1 List where
  eq1 Nil Nil = true
  eq1 Nil _   = false
  eq1 _   Nil = false
  eq1 _   _   = true

else instance eq1GenericEq1
  :: ( Generic1 f rep
     , GenericEq1 rep
     )
  => Eq1 f
  where
  eq1 x y = genericEq1 (from1 x) (from1 y)

class GenericEq1 a where
  genericEq1 :: a -> a -> Boolean

instance genericEq1Sum :: GenericEq1 b => GenericEq1 (Sum a b) where
  genericEq1 (Inl _) (Inl _) = true
  genericEq1 (Inr x) (Inr y) = genericEq1 x y
  genericEq1 _        _      = false

instance genericEq1Product
  :: GenericEq1 a
  => GenericEq1 (Product a b)
  where
  genericEq1 (Product x _) (Product y _) = genericEq1 x y

instance genericEqe1Constructor :: GenericEq1 (Constructor name a) where
  genericEq1 _ _ = true
