module Data.IndexedMultiSet
       ( IndexKey(..)
       , SplitKey(..)

       , IndexedMultiSet
       , multiAt
       , (!)
       , empty
       , lookup
       , insert
       , delete
       , map
       , fromList
       , toList
       ) where

import Data.Maybe
import Data.IndexedSet (IndexKey(..), SplitKey(..))
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as M

newtype IndexedMultiSet k a = IMSet (MultiMap k a)
                            deriving (Show, Eq)

multiAt :: SplitKey k => k -> Lens' (IndexedMultiSet k a) [WithoutKey k a]
multiAt k = lens
            (\(IMSet m) -> map (snd . view splitKey) $ lookup k m)
            (\(IMSet m) vs = IMSet $ mapValues (const $ map (view (from splitKey) . (k, )) vs) k m)
          where -- Ineffective; add "mapValues" directly to the MultiMap!
                mapValues :: ([a] -> [a]) -> k -> MultiMap k a -> MultiMap k a
                mapValues f k m = foldr (M.insert k) (M.delete k m) $ map f $ M.lookup k m

(!) :: Ord k => IndexedMultiSet k a -> k -> [a]
(ISet m) ! k = m M.! k

infixl 9 !

empty :: IndexedMultiSet k a
empty = IMSet M.null

lookup :: Ord k => k -> IndexedMultiSet k a -> [a]
lookup k (IMSet m) = k `M.lookup` m

insert :: IndexKey k a => a -> IndexedSet k a -> IndexedSet k a
insert a (IMSet m) = IMSet $ M.insert (toIndex a) a m

delete :: IndexKey k a => a -> IndexedSet k a -> IndexedSet k a
delete a (IMSet m) = IMSet $ M.delete (toIndex a) m

map :: (SplitKey k a, SplitKey k b) => (WithoutKey k a -> WithoutKey k b) -> IndexedSet k a -> IndexedSet k b
map f (IMSet m) = IMSet $ M.map mapSplitKey m

fromList :: IndexKey k a => [a] -> IndexedMultiSet k a
fromList = ISet . M.fromList . map (\x -> (toIndex x, x))

toList :: IndexedSet k a -> [a]
toList (ISet m) = map snd $ M.toList m
