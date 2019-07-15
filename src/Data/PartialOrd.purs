module Data.PartialOrd
  ( class PartialOrd
  , pBetween
  , pClamp
  , pCompare
  , pComparing
  , pGreaterThan     , (>?)
  , pGreaterThanOrEq , (>=?)
  , pLessThan        , (<?)
  , pLessThanOrEq    , (<=?)
  , pMax
  , pMin
  ) where

import Prelude (map, ($), (<$>), (<*>), (>=>))

import Data.HeytingAlgebra (conj)
import Data.Maybe (Maybe(Just))
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering(EQ, GT, LT))

-- | The `PartialOrd` typeclass registers types that support a partial order.
-- |
-- | `PartialOrd` instances should satisfy the laws of partial orderings:
-- |
-- | - Reflexivity: `a <=? a == Just true`
-- | - Antisymmetry:
-- |     if `a <=? b == Just true` and `b <=? a == Just true`
-- |     then `a == b`
-- | - Transitivity:
-- |     if `a <=? b == Just true` and `b <=? c == Just true`
-- |     then `a <=? c == Just true`
-- |
-- | Although partial orderings and total orderings have similar laws, unlike
-- | total orderings, partial orderings need not satisfy the law of connexity,
-- | which follows:
-- |
-- | - Connexity: `a <= b` or `b <= a`
class PartialOrd a where
  pCompare :: a -> a -> Maybe Ordering

instance partialOrdOrd :: Ord a => PartialOrd a where
  pCompare x y = Just $ compare x y

pBetween :: forall a. PartialOrd a => a -> a -> a -> Maybe Boolean
pBetween low hi x = conj <$> (x >=? low) <*> (x <=? hi)

pClamp :: forall a. PartialOrd a => a -> a -> a -> Maybe a
pClamp low hi = pMax low >=> pMin hi

pComparing :: forall a b. PartialOrd b => (a -> b) -> a -> a -> Maybe Ordering
pComparing f x y = pCompare (f x) (f y)

pGreaterThan :: forall a. PartialOrd a => a -> a -> Maybe Boolean
pGreaterThan a1 a2 = map f $ pCompare a1 a2
  where
  f GT = true
  f _ = false

infixl 4 pGreaterThan as >?

pGreaterThanOrEq :: forall a. PartialOrd a => a -> a -> Maybe Boolean
pGreaterThanOrEq a1 a2 = map f $ pCompare a1 a2
  where
  f LT = false
  f _ = true

infixl 4 pGreaterThanOrEq as >=?

pLessThan :: forall a. PartialOrd a => a -> a -> Maybe Boolean
pLessThan a1 a2 = map f $ pCompare a1 a2
  where
  f LT = true
  f _ = false

infixl 4 pLessThan as <?

pLessThanOrEq :: forall a. PartialOrd a => a -> a -> Maybe Boolean
pLessThanOrEq a1 a2 = map f $ pCompare a1 a2
  where
  f GT = false
  f _ = true

infixl 4 pLessThanOrEq as <=?

pMax :: forall a. PartialOrd a => a -> a -> Maybe a
pMax x y = map f $ pCompare x y
  where
  f EQ = x
  f GT = x
  f LT = y

pMin :: forall a. PartialOrd a => a -> a -> Maybe a
pMin x y = map f $ pCompare x y
  where
  f EQ = x
  f GT = y
  f LT = x
