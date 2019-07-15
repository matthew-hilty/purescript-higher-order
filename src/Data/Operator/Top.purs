module Data.Operator.Top
  ( class Top1
  , class Top1_
  , top1
  , top1_
  ) where

import Prelude (const, (<<<))

import Control.Applicative (pure)
import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Data.CatList (CatList)
import Data.CatQueue (CatQueue)
import Data.Either (Either)
import Data.EitherR (EitherR)
import Data.List (List)
import Data.List.ZipList (ZipList)
import Data.Maybe (Maybe)
import Data.Maybe.First (First(First))
import Data.Maybe.Last (Last(Last))
import Data.Map (Map)
import Data.Map (singleton) as Map
import Data.Set (Set)
import Data.Set (singleton) as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty (singleton) as NonEmptySet
import Data.Top (class Top, top)

-- | The `Top1` typeclass represents type constructors `f` that have a
-- | distinguished element `top1` of type `forall a. f a` as well as an
-- | associated partial ordering (or at least a notion thereof), for which
-- | `top1` is the minimum or lowest bound.
-- |
-- | Because the notion of maximality entails the notion of comparability,
-- | the semantics of an instance of `Top1` must be consistent with the
-- | definitional requirements of a partial order. In fact, many instances of
-- | `Top1` are also instances of `PartialOrd1` (and likely `Ord1` as well).
-- |
-- | In such cases, when a type operator `f` is both a registered instance of
-- | `Top1` and one of `PartialOrd1`, it must satisfy the following law:
-- |
-- | - maximality: `x .<=? top1 == Just true`
-- |
-- | Likewise, if `f` is an instance of `Top1` and also an instance of
-- | `Ord1`, it must satisfy the following analogous law:
-- |
-- | - maximality: `x .<= top1`
-- |
-- | Additionally, if `f` is an instance of a higher-order additive
-- | semigroup-like structure like `Alt`, `f`'s `top1` value must be one of the
-- | structure's annihilating elements:
-- |
-- | - Left annihilation: `top1 + x .== top1`
-- | - Right annihilation: `x + top1 .== top1`
-- |
class Top1 (f :: Type -> Type) where
  top1 :: forall a. f a

-- | The `Top1_` typeclass represents type constructors `f` that have a
-- | distinguished function `top1_` of type `forall a. a -> f a` as well as an
-- | associated partial ordering (or at least a notion thereof), for which the
-- | class of evaluations of `top1_` constitutes the maximum or upper bound.
-- |
-- | Because the notion of maximality entails the notion of comparability,
-- | the semantics of an instance of `Top1_` must be consistent with the
-- | definitional requirements of a partial order. In fact, many instances of
-- | `Top1_` are also instances of `PartialOrd1` (and likely `Ord1` as well).
-- |
-- | In such cases, when a type operator `f` is both a registered instance of
-- | `Top1_` and one of `PartialOrd1`, it must satisfy the following law:
-- |
-- | - maximality: `x .<=? top1_ y == Just true`
-- |
-- | Likewise, if `f` is an instance of `Top1_` and also an instance of
-- | `Ord1`, it must satisfy the following analogous law:
-- |
-- | - maximality: `x .<= top1_ y`
-- |
-- | Additionally, if `f` is an instance of a higher-order additive
-- | semigroup-like structure like `Alt`, `f`'s `top1_` value must be one of
-- | the structure's annihilating elements:
-- |
-- | - Left annihilation: `top1_ x + y .== top1_ x`
-- | - Right annihilation: `x + top1_ y .== top1_ y`
-- |
class Top1_ (f :: Type -> Type) where
  top1_ :: forall a. a -> f a

-- | Commonly, instances of `Applicative` admit a coarse interpratation as a
-- | higher-order bounded meet-semilattice with one or two equivalence classes
-- | such that `pure` serves as the `top1_` upper bound.
-- | However, not all instances of `Applicative` can/should be interpreted this way;
-- | and other less-coarse interpretations may also be possible.
-- | Here's a counter-example:
-- | ```purescript
-- | data Foo a = Low | Mid a | Hi a
-- | instance top1_Foo :: Top1_ Foo where
-- |   top1_ = Hi
-- | instance ord1Foo :: Ord1 Foo where
-- |   compare1 Low     Low     = EQ
-- |   compare1 Low     _       = LT
-- |   compare1 (Mid _) Low     = GT
-- |   compare1 (Mid _) (Mid _) = EQ
-- |   compare1 (Mid _) (Hi _)  = LT
-- |   compare1 (Hi _)  (Hi _)  = EQ
-- |   compare1 (Hi _)  _       = GT
-- | instance functorFoo :: Functor Foo where
-- |   map f (Hi x) = Hi (f x)
-- |   map f (Mid x) = Mid (f x)
-- |   map _ Low = Low
-- | instance applyFoo :: Apply Foo where
-- |   apply :: forall a b. f (a -> b) -> f a -> f b
-- |   apply (Hi f) x = f <$> x
-- |   apply (Mid f) x = f <$> x
-- |   apply Low x = Low
-- | instance applicativeFoo :: Applicative Foo where
-- |   pure = Mid
-- | ```

instance top1_Array         :: Top1_ Array       where top1_ = pure
else instance top1_CatList  :: Top1_ CatList     where top1_ = pure
else instance top1_CatQueue :: Top1_ CatQueue    where top1_ = pure
else instance top1_First    :: Top1_ First       where top1_ = First <<< top1_
else instance top1_Either   :: Top1_ (Either a)  where top1_ = pure
else instance top1_EitherR  :: Top1_ (EitherR a) where top1_ = pure
else instance top1_Last     :: Top1_ Last        where top1_ = Last <<< top1_
else instance top1_List     :: Top1_ List        where top1_ = pure
else instance top1_Maybe    :: Top1_ Maybe       where top1_ = pure
else instance top1_Set      :: Top1_ Set         where top1_ = Set.singleton
else instance top1_ZipList  :: Top1_ ZipList     where top1_ = pure

else instance top1_ExceptT :: Top1_ a => Top1_ (ExceptT e a) where
  top1_ = ExceptT <<< top1_ <<< top1_

else instance top1_Map :: Top k => Top1_ (Map k) where
  top1_ = Map.singleton top

else instance top1_NonEmptySet :: Top1_ NonEmptySet where
  top1_ = NonEmptySet.singleton

else instance top1_Top1 :: Top1 f => Top1_ f where
  top1_ = const top1

-- | The `Top2` typeclass represents type constructors `f` that have a
-- | distinguished function `top2` of type `forall b. a -> f b` (for an
-- | instantiated type parameter `a`) as well as an associated partial
-- | ordering (or at least a notion thereof), for which the class of
-- | evaluations of `top2` constitutes the maximum or upper bound.
-- |
-- | Because the notion of maximality entails the notion of comparability,
-- | the semantics of an instance of `Top2` must be consistent with the
-- | definitional requirements of a partial order. In fact, many instances of
-- | `Top2` are also instances of `PartialOrd1` (and likely `Ord1` as well).
-- |
-- | In such cases, when a type operator `f` is both a registered instance of
-- | `Top2` and one of `PartialOrd1`, it must satisfy the following law:
-- |
-- | - maximality: `x .<=? top2 y == Just true`
-- |
-- | Likewise, if `f` is an instance of `Top2` and also an instance of
-- | `Ord1`, it must satisfy the following analogous law:
-- |
-- | - maximality: `x .<= top2 y`
-- |
-- | Additionally, if `f` is an instance of a higher-order additive
-- | semigroup-like structure like `Alt`, `f`'s `top2` value must be one of
-- | the structure's annihilating elements:
-- |
-- | - Left annihilation: `top2 x + y .== top2 x`
-- | - Right annihilation: `x + top2 y .== top2 y`
-- |
class Top2 (f :: Type -> Type) (a :: Type) where
  top2 :: forall b. a -> f b

instance top2Top1 :: Top1 f => Top2 f a where
  top2 = const top1
