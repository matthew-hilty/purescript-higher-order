module Data.Operator.Bottom
  ( class Bottom1
  , class Bottom1_
  , class Bottom2
  , bottom1
  , bottom1_
  , bottom2
  ) where

import Prelude (const, ($), (<<<))

import Control.Monad (class Monad)
import Control.Monad.Except.Trans (class MonadThrow, ExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Writer.Trans (WriterT)
import Control.Plus (class Plus, empty)
import Data.Either (Either(Left, Right))
import Data.EitherR (EitherR(EitherR))
import Data.Monoid (class Monoid)
import Data.Profunctor.Star (Star(Star))

-- | The `Bottom1` typeclass represents type constructors `f` that have a
-- | distinguished element `bottom1` of type `forall a. f a` as well as an
-- | associated partial ordering (or at least a notion thereof), for which
-- | `bottom1` is the minimum or lowest bound.
-- |
-- | Because the notion of minimality entails the notion of comparability,
-- | the semantics of an instance of `Bottom1` must be consistent with the
-- | definitional requirements of a partial order. In fact, many instances of
-- | `Bottom1` are also instances of `PartialOrd1` (and likely `Ord1` as well).
-- |
-- | In such cases, when a type operator `f` is both a registered instance of
-- | `Bottom1` and one of `PartialOrd1`, it must satisfy the following laws:
-- |
-- | - minimality: `x .>=? bottom1 == Just true`
-- |
-- | Likewise, if `f` is an instance of `Bottom1` and also an instance of
-- | `Ord1`, it must satisfy the following analogous law:
-- |
-- | - minimality: `x .>= bottom1`
-- |
-- | Additionally, higher-order semiring-related laws may also be mandated.
-- | If `f` is an instance of a higher-order additive semigroup-like structure
-- | like `Alt`, `f`'s `bottom1` value must act as the structure's identity:
-- |
-- | - left identity: `bottom1 + x .== x`
-- | - right identity: `x + bottom1 .== x`
-- |
-- | If `f` is an instance of a higher-order multiplicative
-- | semigroup-like structure like `Applicative` or `Monad`, `f`'s `bottom1`
-- | value must act as the structure's zero element:
-- |
-- | - Left zero: `bottom1 * x .== bottom1`
-- | - Right zero: `x * bottom1 .== bottom1`
-- |
class Bottom1 (f :: Type -> Type) where
  bottom1 :: forall a. f a

instance bottom1Star :: Bottom1 f => Bottom1 (Star f a) where
  bottom1 = Star $ const bottom1

else instance bottom1Plus :: Plus f => Bottom1 f where
  bottom1 = empty

-- | The `Bottom1_` typeclass represents type constructors `f` that have a
-- | distinguished function `bottom1_` of type `forall a. a -> f a` as well as
-- | an associated partial ordering (or at least a notion thereof), for which
-- | the class of evaluations of `bottom1_` constitutes the minimum or lowest
-- | bound.
-- |
-- | Because the notion of minimality entails the notion of comparability,
-- | the semantics of an instance of `Bottom1_` must be consistent with the
-- | definitional requirements of a partial order. In fact, many instances of
-- | `Bottom1_` are also instances of `PartialOrd1` (and likely `Ord1` as well).
-- |
-- | In such cases, when a type operator `f` is both a registered instance of
-- | `Bottom1_` and one of `PartialOrd1`, it must satisfy the following laws:
-- |
-- | - minimality: `x .>=? bottom1_ y == Just true`
-- |
-- | Likewise, if `f` is an instance of `Bottom1_` and also an instance of
-- | `Ord1`, it must satisfy the following analogous law:
-- |
-- | - minimality: `x .>= bottom1_ y`
-- |
-- | Additionally, higher-order semiring-related laws may also be mandated.
-- | If `f` is an instance of a higher-order additive semigroup-like structure
-- | like `Alt`, `f`'s `bottom1_` value must act as the structure's identity:
-- |
-- | - left identity: `bottom1_ x + y .== y`
-- | - right identity: `x + bottom1_ y .== x`
-- |
-- | If `f` is an instance of a higher-order multiplicative
-- | semigroup-like structure like `Applicative` or `Monad`, `f`'s `bottom1_`
-- | value must act as the structure's zero element:
-- |
-- | - Left zero: `bottom1_ x * y .== bottom1_ x`
-- | - Right zero: `x * bottom1_ y .== bottom1_ y`
-- |
class Bottom1_ (f :: Type -> Type) where
  bottom1_ :: forall a. a -> f a

instance bottom1_Bottom1 :: Bottom1 f => Bottom1_ f where
  bottom1_ = const bottom1

-- | The `Bottom2` typeclass represents type constructors `f` that have a
-- | distinguished function `bottom2` of type `forall b. a -> f b` (for an
-- | instantiated type parameter `a`) as well as an associated partial
-- | ordering (or at least a notion thereof), for which the class of
-- | evaluations of `bottom2` constitutes the minimum or lowest bound.
-- |
-- | Because the notion of minimality entails the notion of comparability,
-- | the semantics of an instance of `Bottom2` must be consistent with the
-- | definitional requirements of a partial order. In fact, many instances of
-- | `Bottom2` are also instances of `PartialOrd1` (and likely `Ord1` as well).
-- |
-- | In such cases, when a type operator `f` is both a registered instance of
-- | `Bottom2` and one of `PartialOrd1`, it must satisfy the following law:
-- |
-- | - minimality: `x .>=? bottom2 y == Just true`
-- |
-- | Likewise, if `f` is an instance of `Bottom2` and also an instance of
-- | `Ord1`, it must satisfy the following analogous law:
-- |
-- | - minimality: `x .>= bottom2 y`
-- |
-- | Additionally, higher-order semiring-related laws may also be mandated.
-- | If `f` is an instance of a higher-order additive semigroup-like structure
-- | like `Alt`, `f`'s `bottom2` value must act as the structure's identity:
-- |
-- | - left identity: `bottom2 x + y .== y`
-- | - right identity: `x + bottom2 y .== x`
-- |
-- | If `f` is an instance of a higher-order multiplicative
-- | semigroup-like structure like `Applicative` or `Monad`, `f`'s `bottom2`
-- | value must act as the structure's zero element:
-- |
-- | - Left zero: `bottom2 x * y .== bottom2 x`
-- | - Right zero: `x * bottom2 y .== bottom2 y`
-- |
class Bottom2 (f :: Type -> Type) (a :: Type) where
  bottom2 :: forall b. a -> f b

instance bottom2Either :: Bottom2 (Either a) a where
  bottom2 = Left
else instance bottom2EitherR :: Bottom2 (EitherR a) a where
  bottom2 = EitherR <<< Right

-- | Instances of `MonadThrow`:
-- | ==========================
-- | Resolution of typeclass instance chains is not responsive to constraints.
-- | Otherwise, the following schema could be used.
-- |
-- | ```
-- | instance bottom2MonadThrow :: MonadThrow f m a => Bottom2 (f m) a where
-- |   bottom2 = throwError
-- | ```
else instance bottom2ExceptT :: Monad m => Bottom2 (ExceptT a m) a where
  bottom2 = throwError
else instance bottom2MaybeT :: MonadThrow a m => Bottom2 (MaybeT m) a where
  bottom2 = throwError
else instance bottom2ReaderT :: MonadThrow a m => Bottom2 (ReaderT r m) a where
  bottom2 = throwError
else instance bottom2RWST
  :: (MonadThrow a m, Monoid w)
  => Bottom2 (RWST r w s m) a
  where
  bottom2 = throwError
else instance bottom2StateT :: MonadThrow a m => Bottom2 (StateT s m) a where
  bottom2 = throwError
else instance bottom2WriterT
  :: (Monoid w, MonadThrow a m)
  => Bottom2 (WriterT w m) a
  where
  bottom2 = throwError

else instance bottom2Bottom1 :: Bottom1 f => Bottom2 f a where
  bottom2 = const bottom1
