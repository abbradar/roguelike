module Data.IndexedSet
       ( IndexedSet
       , IndexKey(..)
       , (!)
       , fromList
       ) where

import Data.Map (Map)
import qualified Data.Map as M

newtype IndexedSet k a = IndexedSet (Map k a)
                        deriving (Show, Eq)

class Ord k => IndexKey k a where
  toIndex :: a -> k

infixl 9 !

(!) :: Ord k => IndexedSet k a -> k -> a
(IndexedSet m) ! k = m M.! k

lookup :: Ord k => k -> IndexedSet k a -> Maybe a
lookup k (IndexedSet m) = k `M.lookup` m

fromList :: IndexKey k a => [a] -> IndexedSet k a
fromList = IndexedSet . M.fromList . map (\x -> (toIndex x, x))

toList :: IndexedSet k a -> [a]
toList (IndexedSet m) = map snd $ M.toList $ m
