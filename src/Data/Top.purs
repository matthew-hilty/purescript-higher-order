module Data.Top
  ( class Top
  , top
  ) where

import Prelude (const, ($))

import Data.Bounded (class Bounded)
import Data.Bounded (top) as Bounded
import Data.Either (Either(Left, Right))
import Data.EitherR (EitherR(EitherR))
import Data.HeytingAlgebra (class HeytingAlgebra, tt)
import Data.Maybe (Maybe(Just))
import Data.Monoid.Conj (Conj(Conj))
import Data.Monoid.Disj (Disj(Disj))
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.Semiring (class Semiring, one)

-- | The `Top` typeclass registers partially ordered types that have
-- | a greatest element or upper boundary.
-- |
-- | Because the notion of maximality entails the notion of comparability,
-- | the semantics of an instance of `Top` must be consistent with the
-- | definitional requirements of a partial order. In fact, most instances
-- | of `Top` are also instances of `PartialOrd` (and likely `Ord` as well).
-- |
-- | In such cases, when a type `a` is both a registered instance of `Top`
-- | and one of `PartialOrd`, it must satisfy the following law:
-- |
-- | - maximality: `x <=? top == Just true`
-- |
-- | Likewise, if `a` is an instance of `Top` and also an instance of `Ord`,
-- | it must satisfy the following analogous law:
-- |
-- | - maximality: `x <= top`
-- |
-- | However, although the semantics of `Top`-registered types must be
-- | consistent with the laws of partial ordering, a `PartialOrd` registration
-- | may not be possible or suitable for all `Top` instances. The
-- | implementation details of the datatype may preclude an instance
-- | declaration. Alternatively, the properties of the type may make such an
-- | instance declaration impractical. For these reasons, as well as for
-- | consistency with the `Bottom` typeclass, the `Top` typeclass does not
-- | declare `PartialOrd` as a prerequisite superclass.
-- |
class Top (a :: Type) where
  top :: a

instance topFunction :: Top b => Top (a -> b) where
  top = const top

-- | Instances of `Top1_`:
-- | =====================
-- | Resolution of typeclass instance chains is not responsive to constraints.
-- | Otherwise, the following schema could be used.
-- |
-- | ```
-- | else instance topTop1 :: (Top a, Top1_ f) => Top (f a) where
-- |   top = top1_ top
-- | ```
else instance topMaybe :: Top a => Top (Maybe a) where
  top = Just top

-- | Instances of `Top2`:
-- | ====================
-- | Resolution of typeclass instance chains is not responsive to constraints.
-- | Otherwise, the following schema could be used.
-- |
-- | ```
-- | else instance topTop2 :: (Top a, Top2 f a) => Top (f b a) where
-- |   top = top2 top
-- | ```
else instance topEither :: Top b => Top (Either a b) where
  top = Right top

else instance topEitherR :: Top b => Top (EitherR a b) where
  top = EitherR $ Left top

-- | Instances of `Semiring`:
-- | ========================
-- | The `Semiring` constraint itself is not relevant. However, when
-- | `Multiplicative` or `Additive` wraps a `Semiring` instance, the semiring's
-- | `one` value serves as `top`.
-- |
-- | Note that an unwrapped value of a semiring `a` may also satisfy the
-- | `Top` constraint; however, its `top` value could be altogether
-- | different. This might happen, for example, if `a` is not only an instance
-- | of `Semiring` but also an instance of `Bounded`.
else instance topAdditive :: Semiring a => Top (Additive a) where
  top = Additive one

else instance topMultiplicative :: Semiring a => Top (Multiplicative a) where
  top = Multiplicative one

-- | Instances of `HeytingAlgebra`:
-- | ==============================
-- | The `HeytingAlgebra` constraint itself is not relevant. However, when
-- | `Conj` or `Disj` wraps a `HeytingAlgebra` instance, the Heyting algebra's
-- | `tt` value serves as `top`.
-- |
-- | Note that an unwrapped value of a Heyting algebra `a` may also satisfy the
-- | `Top` constraint; however, its `top` value could be altogether
-- | different. This might happen, for example, if `a` is not only an instance
-- | of `HeytingAlgebra` but also an instance of `Bounded`.
else instance topConj :: HeytingAlgebra a => Top (Conj a) where top = Conj tt
else instance topDisj :: HeytingAlgebra a => Top (Disj a) where top = Disj tt

-- | Instances of `Bounded`:
-- | =======================
-- | `Bounded` instances are necessarily instances of `Top`.
else instance topBounded :: Bounded a => Top a where
  top = Bounded.top
