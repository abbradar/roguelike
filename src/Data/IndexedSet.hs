module Data.IndexedSet
       ( IndexKey(..)
       , SplitKey(..)
         
       , IndexedSet
       , (!)
       , empty
       , lookup
       , insert
       , delete
       , itraverseSet
       , fromList
       , toList
       ) where

import Control.Arrow
import Control.Lens
import Data.Proxy
import Data.Map (Map)
import qualified Data.Map as M

class IndexKey k a where
  toIndex :: a -> k
  default toIndex :: SplitKey k a => a -> k
  toIndex = fst . view splitKey

class IndexKey k a => SplitKey k a where
  type WithoutKey k a
  splitKey :: Iso' a (k, WithoutKey k a)

newtype IndexedSet k a = ISet (Map k a)
                        deriving (Show, Eq)

_indexedSet :: Lens (IndexedSet k a) (IndexedSet k' a') (Map k a) (Map k' a')
_indexedSet = lens (\(ISet x) -> x) ISet

type instance Index (IndexedSet k a) = k
type instance IxValue (IndexedSet k a) = WithoutKey k a

instance SplitKey k a => Ixed (IndexedSet k a) where
  ix k = _indexedSet . ix k . splitKey . _2

instance SplitKey k a => At (IndexedSet k a) where
  at k = _indexedSet . at k . lens (fmap $ snd . view splitKey) (const $ fmap $ view (from splitKey) . (k, ))

empty :: IndexedMultiSet k a
empty = IMSet M.null

(!) :: Ord k => IndexedSet k a -> k -> a
(ISet m) ! k = m M.! k

infixl 9 !

empty :: IndexedSet k a
empty = IndexedSet M.empty

lookup :: Ord k => k -> IndexedMultiSet k a -> [a]
lookup k (IMSet m) = k `M.lookup` m

insert :: (Ord k, IndexKey k a) => a -> IndexedSet k a -> IndexedSet k a
insert a (IMSet m) = ISet $ M.insert (toIndex a) a m

delete :: (Ord k, IndexKey k a) => a -> IndexedSet k a -> IndexedSet k a
delete a (IMSet m) = ISet $ M.delete (toIndex a) m

itraverseSet :: (SplitKey k a, SplitKey k b) => IndexedTraversal k (IndexedSet k a) (IndexedSet k b) (WithoutKey k a) (WithoutKey k b)
itraverseSet = _indexedSet . itraverse . splitKey . _2

fromList :: (Ord k, IndexKey k a) => [a] -> IndexedSet k a
fromList = ISet . M.fromList . map (\x -> (toIndex x, x))

toList :: IndexedSet k a -> [a]
toList (ISet m) = map snd $ M.toList m
