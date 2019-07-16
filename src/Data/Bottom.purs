module Data.Bottom
  ( class Bottom
  , bottom
  ) where

import Prelude (const, ($))

import Data.Bounded (class Bounded)
import Data.Bounded (bottom) as Bounded
import Data.CatList (CatList)
import Data.CatQueue (CatQueue)
import Data.Either (Either(Left, Right))
import Data.EitherR (EitherR(EitherR))
import Data.HeytingAlgebra (class HeytingAlgebra, ff)
import Data.List (List)
import Data.List.ZipList (ZipList)
import Data.Maybe (Maybe(Nothing))
import Data.Maybe.First (First(First))
import Data.Maybe.Last (Last(Last))
import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Monoid (mempty)
import Data.Monoid.Conj (Conj(Conj))
import Data.Monoid.Disj (Disj(Disj))
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.Semiring (class Semiring, zero)
import Data.Set (Set)
import Data.Set (empty) as Set

-- | The `Bottom` typeclass registers partially ordered types that have
-- | a least element or lower boundary.
-- |
-- | Because the notion of minimality entails the notion of comparability,
-- | the semantics of an instance of `Bottom` must be consistent with the
-- | definitional requirements of a partial order. In fact, most instances
-- | of `Bottom` are also instances of `PartialOrd` (and likely `Ord` as well).
-- |
-- | In such cases, when a type `a` is both a registered instance of `Bottom`
-- | and one of `PartialOrd`, it must satisfy the following law:
-- |
-- | - minimality: `x >=? bottom == Just true`
-- |
-- | Likewise, if `a` is an instance of `Bottom` and also an instance of `Ord`,
-- | it must satisfy the following analogous law:
-- |
-- | - minimality: `x >= bottom`
-- |
-- | However, although the semantics of `Bottom`-registered types must be
-- | consistent with the laws of partial ordering, a `PartialOrd` registration
-- | may not be possible or suitable for all `Bottom` instances. The
-- | implementation details of the datatype may preclude an instance
-- | declaration. Alternatively, the properties of the type may make such an
-- | instance declaration impractical.
-- |
-- | For example, many container-like datatype operators support a total order
-- | (therefore, also a partial order) when they act on total orders.
-- | `Array Int`, for instance, is a total order because `Int` is a total
-- | order. However, the `bottom` element of such container types is generally
-- | independent of the type argument of the container's type constructor. The
-- | `bottom` element of `Array a`, for instance, is `[]` whether `a` is `Int`,
-- | `String`, any other `Ord` instance, or, conceivably, any other type at
-- | all. That is to say, `Array a`, for any unconstrained `a`, satisfies
-- | `Bottom`. Were a `PartialOrd` registration a prerequisite of `Bottom`-
-- | instance declarations, some choice of `PartialOrd` implementation
-- | satisfiable by `Array a` for all `a` would be necessary. Such an
-- | implementation might be the following:
-- |
-- | ```
-- | -- # 1
-- | instance partialOrdArray :: PartialOrd (Array a) where
-- |   pCompare x y
-- |     | null x, null y -> Just EQ
-- |     | null x         -> Just LT
-- |     | null y         -> Just GT
-- |     | otherwise      -> Nothing
-- | ```
-- |
-- | This choice is, however, rather coarse for likely uses of partially
-- | orderable arrays. Arrays often contain values of types satisfying `Ord`,
-- | and a better `PartialOrd` definition for these kinds of arrays might be
-- | the following:
-- |
-- | ```
-- | -- # 2
-- | instance partialOrdArray :: Ord a => PartialOrd (Array a) where
-- |   pCompare x y = Just $ compare x y
-- | ```
-- |
-- | The existence of two reasonable `PartialOrd` definitions for `Array a`
-- | suggests that preferring more-granular definitions whenever possible,
-- | while using #1 above as a backup implementation would be a useful general
-- | strategy. Instance chaining is the standard technique in PureScript to
-- | effect this kind of typeclass-instance contingency. However, the current
-- | strategy of instance resolution by the PureScript compiler rules out
-- | instance chaining in cases like this. Because both declarations in the
-- | would-be chain (see below) share the same head (namely, `Array`), the
-- | second definition (the backup definition) would not be checked or attempted.
-- |
-- | ```
-- | instance partialOrdArray :: Ord a => PartialOrd (Array a) where
-- |   pCompare x y = Just $ compare x y
-- | else instance partialOrdArray :: PartialOrd (Array a) where
-- |   pCompare x y
-- |     | null x, null y -> Just EQ
-- |     | null x         -> Just LT
-- |     | null y         -> Just GT
-- |     | otherwise      -> Nothing
-- | ```
-- |
-- | Therefore, to make `Bottom` registrations of unconstrained `Array a` and
-- | other similar types possible, without also mandating compiler-ignored (and
-- | possibly idle) `PartialOrd` definitions, the `Bottom` typeclass does not
-- | declare `PartialOrd` as a prerequisite superclass.
-- |
class Bottom (a :: Type) where
  bottom :: a

instance bottomFunction :: Bottom b => Bottom (a -> b) where bottom = const bottom
else instance bottomMap :: Bottom (Map k v)            where bottom = Map.empty

-- | Instances of `Monoid`:
-- | ======================
-- | Many instances of `Monoid` are also instances of `Bottom`.
-- | Often, definitions of `bottom` and `mempty` are equivalent.
-- | However, equivalence is not necessary. `Ordering` is an example of a data
-- | type that satisfies both `Bottom` and `Monoid`, but whereas `Ordering`'s
-- | lower bound is `LT`, its monoidal identity is `EQ`.
else instance bottomString :: Bottom String where bottom = mempty

-- | Instances of `Bottom1`:
-- | =======================
-- | Resolution of typeclass instance chains is not responsive to constraints.
-- | Otherwise, the following schema could be used.
-- |
-- | ```
-- | else instance bottomBottom1 :: Bottom1 f => Bottom (f a) where
-- |   bottom = bottom1
-- | ```
else instance bottomArray    :: Bottom (Array a)    where bottom = mempty
else instance bottomCatList  :: Bottom (CatList a)  where bottom = mempty
else instance bottomCatQueue :: Bottom (CatQueue a) where bottom = mempty
else instance bottomFirst    :: Bottom (First a)    where bottom = First Nothing
else instance bottomLast     :: Bottom (Last a)     where bottom = Last Nothing
else instance bottomList     :: Bottom (List a)     where bottom = mempty
else instance bottomMaybe    :: Bottom (Maybe a)    where bottom = Nothing
else instance bottomSet      :: Bottom (Set a)      where bottom = Set.empty
else instance bottomZipList  :: Bottom (ZipList a)  where bottom = mempty

-- | Instances of `Bottom2`:
-- | =======================
-- | Resolution of typeclass instance chains is not responsive to constraints.
-- | Otherwise, the following schema could be used.
-- |
-- | ```
-- | else instance bottomBottom2 :: (Bottom a, Bottom2 f a) => Bottom (f b a) where
-- |   bottom = bottom2 bottom
-- | ```
else instance bottomEither :: Bottom a => Bottom (Either a b) where
  bottom = Left bottom

else instance bottomEitherR :: Bottom a => Bottom (EitherR a b) where
  bottom = EitherR $ Right bottom

-- | Instances of `Semiring`:
-- | ========================
-- | The `Semiring` constraint itself is not relevant. However, when
-- | `Multiplicative` or `Additive` wraps a `Semiring` instance, the semiring's
-- | `zero` value serves as `bottom`.
-- |
-- | Note that an unwrapped value of a semiring `a` may also satisfy the
-- | `Bottom` constraint; however, its `bottom` value could be altogether
-- | different. This might happen, for example, if `a` is not only an instance
-- | of `Semiring` but also an instance of `Bounded`.
else instance bottomAdditive :: Semiring a => Bottom (Additive a) where
  bottom = Additive zero

else instance bottomMultiplicative
  :: Semiring a
  => Bottom (Multiplicative a)
  where
  bottom = Multiplicative zero

-- | Instances of `HeytingAlgebra`:
-- | ==============================
-- | The `HeytingAlgebra` constraint itself is not relevant. However, when
-- | `Conj` or `Disj` wraps a `HeytingAlgebra` instance, the Heyting algebra's
-- | `ff` value serves as `bottom`.
-- |
-- | Note that an unwrapped value of a Heyting algebra `a` may also satisfy the
-- | `Bottom` constraint; however, its `bottom` value could be altogether
-- | different. This might happen, for example, if `a` is not only an instance
-- | of `HeytingAlgebra` but also an instance of `Bounded`.
else instance bottomConj :: HeytingAlgebra a => Bottom (Conj a) where
  bottom = Conj ff

else instance bottomDisj :: HeytingAlgebra a => Bottom (Disj a) where
  bottom = Disj ff

-- | Instances of `Bounded`:
-- | =======================
-- | `Bounded` instances are necessarily instances of `Bottom`.
else instance bottomBounded :: Bounded a => Bottom a where
  bottom = Bounded.bottom
