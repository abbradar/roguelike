module Roguelike.WorldMap ( WorldMap
                          , iptraverse
                          , byPoint
                          , nearbyObjects
                          , fromList
                          ) where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.Aeson
import Control.Lens

import Roguelike.ID
import Roguelike.Types

data MapObject a = MapObject !Point !a

-- Most common world map operations would be:
-- 1) Search by ID
-- 2) Search all entities near a Point
-- TODO: More effectiveness!
--       Add a simple Point index 
data WorldMap a = WorldMap { _objects :: Map id (Point, a)
                           }
                deriving (Show, Generic, Default)

makeLenses ''WorldMap

-- Do not expose indices here, only plain data!
instance (FromJSON id, FromJSON a) => FromJSON (WorldMap id a) where
  parseJSON = liftM fromList . parseJSON

instance (ToJSON id, ToJSON a) => ToJSON (WorldMap id a) where
  toJSON = toJSON . itoList

instance Functor (WorldMap id) where
  fmap = imap . const

instance FunctorWithIndex id (WorldMap id) where

instance Foldable (WorldMap id) where
  foldMap = ifoldMap . const

instance FoldableWithIndex id (WorldMap id) where

instance Traversable (WorldMap id) where
  traverse = itraverse . const

instance TraversableWithIndex id (WorldMap id) where
  itraverse = objects . itraverse . _2

type instance Index (WorldMap id) = id
type instance IxValue (WorldMap k a) = (Point, a)

instance Ixed (WorldMap k a) where
  ix k = objects . ix k

instance At (IndexedSet k a) where
  at k = objects . at k

iptraverse :: Traversal (WorldMap id a) (WorldMap id b) (Point, a) (Point, b)
iptraverse = objects . itraverse

filterPoint :: (Point -> Bool) -> WorldMap id a -> [(id, a)]
-- Slow!
filterPoint f = filter (first p) . M.toList . _objects

byPoint :: Point -> WorldMap id a -> [(id, a)]
byPoint p = filterPoint (== p)

nearbyObjects :: Point -> Radius -> WorldMap id a -> [(id, a)]
nearbyObjects p r = filterPoint (near r p)

fromList :: [(id, (Point, a))] -> WorldMap id a
fromList = WorldMap . M.fromList
