module Data.Operator.Ord
  ( class Ord1
  , between1
  , clamp1
  , compare1
  , comparing1
  , greaterThan1     , (.>)
  , greaterThanOrEq1 , (.>=)
  , lessThan1        , (.<)
  , lessThanOrEq1    , (.<=)
  , max1
  , min1
  ) where

import Prelude (($), (<<<))

import Control.Monad (class Monad)
import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Data.CatList (CatList(CatNil))
import Data.CatQueue (CatQueue(CatQueue))
import Data.Const (Const)
import Data.Either (Either(Left, Right))
import Data.EitherR (EitherR(EitherR))
import Data.Identity (Identity)
import Data.List (List(Nil))
import Data.List.Lazy (null) as LazyList
import Data.List.Lazy.Types (List) as Lazy
import Data.List.ZipList (ZipList(ZipList))
import Data.Maybe (Maybe(Nothing))
import Data.Maybe.First (First(First))
import Data.Maybe.Last (Last(Last))
import Data.Map (Map)
import Data.Map (isEmpty) as Map
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering(EQ, GT, LT))
import Data.Set (Set)
import Data.Set (isEmpty) as Set
import Data.Set.NonEmpty (NonEmptySet)

-- | The `Ord1` typeclass of this module represents type constructors `f` that
-- | have an associated total ordering of values of type `f a` independent of
-- | any choice of `a`.
-- |
-- | Unlike the `Data.Ord.Ord1` typeclass of the 'Prelude' module [hereafter,
-- | qualified by the prefix "Prelude"], `Ord1`, defined here, is not generally
-- | equivalent to the `Ord` typeclass (also found in the 'Prelude' module).
-- |
-- | For example, whereas both
-- |
-- |  `compare (Cons 0 Nil) (Cons 1 Nil) == LT`
-- |
-- | and
-- |
-- |  `Prelude.compare1 (Cons 0 Nil) (Cons 1 Nil) == LT`
-- |
-- | the definition found in this module gives the following:
-- |
-- |  `compare1 (Cons 0 Nil) (Cons 1 Nil) == EQ`
-- |
-- | That is, the `Int` values in a value of `List Int` (and, more generally,
-- | the `a` values in any `List a`) have little relevance in determining the
-- | results of the `compare1` function.
-- |
-- | The instances of the `Ord1` typeclass of this module are designed to
-- | emphasize the value-level effects of the type constructors themselves.
-- | In practice, this often means that an ordering is assigned to the type
-- | constructor's data constructors. The constructors `Cons` and `Nil` of
-- | `List`, for example, effectively constitute the 2-element total order
-- | `Nil < Cons`. The function `compare1`, then, in comparing any pair of
-- | lists, essentially compares the lists' head projections.
-- |
-- | This interpretation of `Ord1` exists in addition to `Prelude.Ord1`
-- | because datatypes often have intrinsic (or designed) subgroupings,
-- | commonly distingished by data constructors, and a means to relate these
-- | different subgroupings can be useful. `Either a b` is a canonical example;
-- | it has two subclasses: a lower band of values of type `a` and an upper
-- | band of values of type `b`. This bifurcation is of particular significance
-- | to the `Alt` typeclass of the 'control' package, since the definition of
-- | its member `alt` (for `Either`) is almost exclusively determined by
-- | `Either`'s `Left` and `Right` data constructors alone.
-- |
-- | Incidentally, although all higher-order semigroups satisfy the type
-- | signature of `alt`, in practice, instances of `Alternative` tend to
-- | behave like totally ordered join-semilattices (usually with just two
-- | equivalence classes, one representing a valence reserved for errors or
-- | deficiency of some kind and the second comprising all remaining valences
-- | of a type constructor). These higher-order join-semilattices, furthermore,
-- | can be derived from the simpler concept of `Ord1`, since `Alt`'s join
-- | operation (`alt`), for such cases, is equivalent to `max1`, an
-- | accompanying utility of `Ord1`.
-- |
class Ord1 f where
  compare1 :: forall a. f a -> f a -> Ordering

between1 :: forall a f. Ord1 f => f a -> f a -> f a -> Boolean
between1 low hi x
  | x .< low = false
  | x .> hi  = false
  | true     = true

clamp1 :: forall a f. Ord1 f => f a -> f a -> f a -> f a
clamp1 low hi = min1 hi <<< max1 low

comparing1
  :: forall a b f
   . Ord1 f
  => (a -> f b)
  -> a
  -> a
  -> Ordering
comparing1 f x y = compare1 (f x) (f y)

greaterThan1 :: forall a f. Ord1 f => f a -> f a -> Boolean
greaterThan1 a1 a2 = f $ compare1 a1 a2
  where
  f GT = true
  f _ = false

infixl 4 greaterThan1 as .>

greaterThanOrEq1 :: forall a f. Ord1 f => f a -> f a -> Boolean
greaterThanOrEq1 a1 a2 = f $ compare1 a1 a2
  where
  f LT = false
  f _ = true

infixl 4 greaterThanOrEq1 as .>=

lessThan1 :: forall a f. Ord1 f => f a -> f a -> Boolean
lessThan1 a1 a2 = f $ compare1 a1 a2
  where
  f LT = true
  f _ = false

infixl 4 lessThan1 as .<

lessThanOrEq1 :: forall a f. Ord1 f => f a -> f a -> Boolean
lessThanOrEq1 a1 a2 = f $ compare1 a1 a2
  where
  f GT = false
  f _ = true

infixl 4 lessThanOrEq1 as .<=

max1 :: forall a f. Ord1 f => f a -> f a -> f a
max1 x y = f $ compare1 x y
  where
  f LT = y
  f EQ = x
  f GT = x

min1 :: forall a f. Ord1 f => f a -> f a -> f a
min1 x y = f $ compare1 x y
  where
  f LT = x
  f EQ = x
  f GT = y

instance ord1Array :: Ord1 Array where
  compare1 [] [] = EQ
  compare1 [] _  = LT
  compare1 _  [] = GT
  compare1 _  _  = EQ

instance ord1CatList :: Ord1 CatList where
  compare1 CatNil CatNil = EQ
  compare1 CatNil _      = LT
  compare1 _      CatNil = GT
  compare1 _      _      = EQ

instance ord1CatQueue :: Ord1 CatQueue where
  compare1 (CatQueue Nil Nil) (CatQueue Nil Nil) = EQ
  compare1 (CatQueue Nil Nil) _                  = LT
  compare1 _                  (CatQueue Nil Nil) = GT
  compare1 _                  _                  = EQ

instance ord1Const :: Ord1 (Const a) where
  compare1 _ _ = EQ

instance ord1First :: Ord1 First where
  compare1 (First x) (First y) = compare1 x y

instance ord1Either :: Ord a => Ord1 (Either a) where
  compare1 (Left x)  (Left y)  = compare x y
  compare1 (Left _)  (Right _) = LT
  compare1 (Right _) (Left _)  = GT
  compare1 (Right _) (Right _) = EQ

instance ord1EitherR :: Ord a => Ord1 (EitherR a) where
  compare1 (EitherR (Left _))  (EitherR (Left _))  = EQ
  compare1 (EitherR (Left _))  (EitherR (Right _)) = LT
  compare1 (EitherR (Right _)) (EitherR (Left _))  = GT
  compare1 (EitherR (Right x)) (EitherR (Right y)) = compare x y

instance ord1ExceptT :: (Monad m, Ord1 m) => Ord1 (ExceptT a m) where
  compare1 (ExceptT x) (ExceptT y) = compare1 x y

instance ord1Identity :: Ord1 Identity where
  compare1 _ _ = EQ

instance ord1Last :: Ord1 Last where
  compare1 (Last x) (Last y) = compare1 x y

instance ord1LazyList :: Ord1 Lazy.List where
  compare1 x y = compare (LazyList.null x) (LazyList.null y)

instance ord1List :: Ord1 List where
  compare1 Nil Nil = EQ
  compare1 Nil _   = LT
  compare1 _   Nil = GT
  compare1 _   _   = EQ

instance ord1Map :: Ord1 (Map k) where
  compare1 x y = compare (Map.isEmpty x) (Map.isEmpty y)

instance ord1Maybe :: Ord1 Maybe where
  compare1 Nothing Nothing = EQ
  compare1 Nothing _       = LT
  compare1 _       Nothing = GT
  compare1 _       _       = EQ

instance ord1NonEmptySet :: Ord1 NonEmptySet where
  compare1 _ _ = EQ

instance ord1Set :: Ord1 Set where
  compare1 x y = compare (Set.isEmpty x) (Set.isEmpty y)

instance ord1ZipList :: Ord1 ZipList where
  compare1 (ZipList x) (ZipList y) = compare1 x y
