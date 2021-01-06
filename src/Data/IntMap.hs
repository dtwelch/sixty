{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}
module Data.IntMap where

import Protolude hiding (IntMap, IntSet)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Coerce
import qualified "containers" Data.IntMap.Lazy as Containers

newtype IntMap key value = IntMap (Containers.IntMap value)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Read, Show, Semigroup, Monoid)

instance (Hashable key, Hashable value, Coercible Containers.Key key) => Hashable (IntMap key value) where
  hashWithSalt salt (IntMap m) = hashWithSalt salt (coerce $ Containers.toList m :: [(key, value)])

null :: IntMap key value -> Bool
null (IntMap m) =
  Containers.null m

singleton :: Coercible key Containers.Key => key -> value -> IntMap key value
singleton key value =
  IntMap $ Containers.singleton (coerce key) value

fromSet :: Coercible key Containers.Key => (key -> value) -> IntSet key -> IntMap key value
fromSet f (IntSet.IntSet set) =
  IntMap $ Containers.fromSet (coerce f) set

fromList :: Coercible key Containers.Key => [(key, value)] -> IntMap key value
fromList xs =
  IntMap $ Containers.fromList $ coerce xs

fromListWith :: Coercible key Containers.Key => (value -> value -> value) -> [(key, value)] -> IntMap key value
fromListWith f xs =
  IntMap $ Containers.fromListWith f $ coerce xs

toList :: Coercible key Containers.Key => IntMap key value -> [(key, value)]
toList (IntMap m) =
  coerce $ Containers.toList m

insert :: Coercible key Containers.Key => key -> value -> IntMap key value -> IntMap key value
insert key value =
  coerce $ Containers.insert (coerce key) value

insertWith :: Coercible key Containers.Key => (value -> value -> value) -> key -> value -> IntMap key value -> IntMap key value
insertWith f key value =
  coerce $ Containers.insertWith f (coerce key) value

adjust :: Coercible key Containers.Key => (value -> value) -> key -> IntMap key value -> IntMap key value
adjust f key =
  coerce $ Containers.adjust f (coerce key)

lookup :: Coercible key Containers.Key => key -> IntMap key value -> Maybe value
lookup key m =
  Containers.lookup (coerce key) (coerce m)

(!) :: Coercible key Containers.Key => IntMap key value -> key -> value
m ! key =
  coerce m Containers.! coerce key

lookupDefault :: Coercible key Containers.Key => value -> key -> IntMap key value -> value
lookupDefault def key m =
  Containers.findWithDefault def (coerce key) (coerce m)

delete :: Coercible key Containers.Key => key -> IntMap key value -> IntMap key value
delete key (IntMap m) =
  coerce $ Containers.delete (coerce key) m

member :: Coercible key Containers.Key => key -> IntMap key value -> Bool
member key (IntMap m) =
  Containers.member (coerce key) m

unionWith :: (value -> value -> value) -> IntMap key value -> IntMap key value -> IntMap key value
unionWith f m1 m2 =
  IntMap $ Containers.unionWith f (coerce m1) (coerce m2)

traverseWithKey :: (Coercible key Containers.Key, Applicative t) => (key -> value1 -> t value2) -> IntMap key value1 -> t (IntMap key value2)
traverseWithKey f (IntMap m) =
  IntMap <$> Containers.traverseWithKey (coerce f) m

keys :: (Coercible key Containers.Key) => IntMap key value -> [key]
keys (IntMap m) =
  coerce $ Containers.keys m

elems :: IntMap key value -> [value]
elems (IntMap m) =
  Containers.elems m
