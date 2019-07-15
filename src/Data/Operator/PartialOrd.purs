module Data.Operator.PartialOrd
  ( class PartialOrd1
  , pBetween1
  , pClamp1
  , pCompare1
  , pComparing1
  , pGreaterThan1     , (.>?)
  , pGreaterThanOrEq1 , (.>=?)
  , pLessThan1        , (.<?)
  , pLessThanOrEq1    , (.<=?)
  , pMax1
  , pMin1
  ) where

import Prelude (map, ($), (<$>), (<*>), (>=>))

import Data.HeytingAlgebra (conj)
import Data.Maybe (Maybe(Just))
import Data.Operator.Ord (class Ord1, compare1)
import Data.Ordering (Ordering(..))

-- | The `PartialOrd1` typeclass represents type constructors `f` that have
-- | an asociated partial ordering of values of type `f a` independent of any
-- | choice of `a`.
-- |
-- | `PartialOrd1` instances should satisfy the laws of partial orderings:
-- |
-- | - Reflexivity: `a .<=? a == Just true`
-- | - Antisymmetry:
-- |     if `a .<=? b == Just true` and `b .<=? a == Just true`
-- |     then `a .== b`
-- | - Transitivity:
-- |     if `a .<=? b == Just true` and `b .<=? c == Just true`
-- |     then `a .<=? c == Just true`
-- |
-- | That is, partial orderings should satisfy all the laws of total orderings
-- |
-- | - Reflexivity: `a .<= a`
-- | - Antisymmetry: if `a .<= b` and `b .<= a` then `a .== b`
-- | - Transitivity: if `a .<= b` and `b .<= c` then `a .<= c`
-- |
-- | ... EXCEPT the law of connexity, which follows:
-- |
-- | - Connexity: `a .<= b` or `b .<= a`
-- |
class PartialOrd1 f where
  pCompare1 :: forall a. f a -> f a -> Maybe Ordering

instance partialOrd1Ord1 :: Ord1 f => PartialOrd1 f where
  pCompare1 x y = Just $ compare1 x y

pBetween1 :: forall a f. PartialOrd1 f => f a -> f a -> f a -> Maybe Boolean
pBetween1 low hi x = conj <$> (x .>=? low) <*> (x .<=? hi)

pClamp1 :: forall a f. PartialOrd1 f => f a -> f a -> f a -> Maybe (f a)
pClamp1 low hi = pMax1 low >=> pMin1 hi

pComparing1
  :: forall a b f
   . PartialOrd1 f
  => (a -> f b)
  -> a
  -> a
  -> Maybe Ordering
pComparing1 f x y = pCompare1 (f x) (f y)

pGreaterThan1 :: forall a f. PartialOrd1 f => f a -> f a -> Maybe Boolean
pGreaterThan1 a1 a2 = map f $ pCompare1 a1 a2
  where
  f GT = true
  f _ = false

infixl 4 pGreaterThan1 as .>?

pGreaterThanOrEq1 :: forall a f. PartialOrd1 f => f a -> f a -> Maybe Boolean
pGreaterThanOrEq1 a1 a2 = map f $ pCompare1 a1 a2
  where
  f LT = false
  f _ = true

infixl 4 pGreaterThanOrEq1 as .>=?

pLessThan1 :: forall a f. PartialOrd1 f => f a -> f a -> Maybe Boolean
pLessThan1 a1 a2 = map f $ pCompare1 a1 a2
  where
  f LT = true
  f _ = false

infixl 4 pLessThan1 as .<?

pLessThanOrEq1 :: forall a f. PartialOrd1 f => f a -> f a -> Maybe Boolean
pLessThanOrEq1 a1 a2 = map f $ pCompare1 a1 a2
  where
  f GT = false
  f _ = true

infixl 4 pLessThanOrEq1 as .<=?

pMax1 :: forall a f. PartialOrd1 f => f a -> f a -> Maybe (f a)
pMax1 x y = map f $ pCompare1 x y
  where
  f LT = y
  f EQ = x
  f GT = x

pMin1 :: forall a f. PartialOrd1 f => f a -> f a -> Maybe (f a)
pMin1 x y = map f $ pCompare1 x y
  where
  f LT = x
  f EQ = x
  f GT = y
