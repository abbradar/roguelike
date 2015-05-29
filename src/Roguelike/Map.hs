module Roguelike.Map ( GlobalID(..)
                     , LocalID(..)
                     , Entity(..)
                     , MapEntity(..)
                     , WorldMap
                     , objectById
                     , objectByPoint
                     , mobjectById
                     , mobjectByPoint
                     , moveEntity
                     , insertEntity
                     , nearbyEntities
                     ) where

import Data.Maybe
import Debug.Trace
import Control.Exception
import Control.Applicative
import Data.Foldable
import Data.List
import Data.IndexedSet (IndexedSet, IndexKey(..))
import qualified Data.IndexedSet as I
import Data.IndexedMultiSet (IndexedMultiSet)
import qualified Data.IndexedMultiSet as M
import Data.Aeson
import Data.Aeson.TH
import Control.Lens

import Roguelike.ID
import Roguelike.Creature
import Roguelike.Item

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Object

data MapObject id a = MapObject { _mapId :: !id
                                , _mapPoint :: !Point
                                , _mapValue :: a
                                }
                    deriving (Show)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''MapObject
makeLenses ''MapObject

data WorldMap id a = WorldMap { _idIndex :: IndexedSet id (MapObject a)
                              , _pointIndex :: IndexedMultiSet id (MapObject a)
                              }
                 deriving (Show)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''WorldMap
makeLenses ''WorldMap

instance IndexKey id (MapObject id a) where

instance SplitKey id (MapObject id a) where
  type WithoutKey = (Point, a)
  splitKey = iso (\m -> (_mapId m, (_mapPoint m, _mapValue m))) (\(id, (p, v)) -> MapObject id p v)

instance IndexKey Point (MapObject id a) where

instance SplitKey Point (MapObject id a) where
  type WithoutKey = (id, a)
  splitKey = iso (\m -> (_mapPoint m, (_mapId m, _mapValue m))) (\(p, (id, v)) -> MapObject id p v)

type instance Index (WorldMap id) = id

instance Functor (WorldMap id) where
  fmap = imap . const

instance FunctorWithIndex id (WorldMap id) where

instance Foldable (WorldMap id) where
  foldMap = ifoldMap . const

instance FoldableWithIndex id (WorldMap id) where

instance Traversable (WorldMap id) where
  traverse = itraverse . const

instance TraversableWithIndex id (WorldMap id) where
  itraverse f m = a
  -- traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

updateObject :: (id, Point) -> a -> WorldMap id a -> WorldMap id a
updateObject (id, p) a w = w & idIndex %~ (I.insert id a)
                             & pointIndex %~ (multiAt p %~ updatePoint)
  where updatePoint [] = [(id, w)]
        updatePoint (h@(id', _):t)
          | id == id' = (id, a):t
          | otherwise = h:updatePoint t

byId :: GlobalID -> Lens' (WorldMap id a) (WithoutKey id a)
byId id = lens (view $ idIndex . 

byPoint :: Point -> WorldMap a -> [WithoutKey Point a]
byPoint =objectBySmth _point

mobjectById :: GlobalID -> Getter (WorldMap a) (Maybe (MapObject id a))
mobjectById = mobjectBySmth _id

mobjectByPoint :: GlobalID -> Getter (WorldMap a) (Maybe (MapObject id a))
mobjectByPoint = mobjectBySmth _point

moveObject :: GlobalID -> Point -> WorldMap a -> WorldMap a
moveObject gid p w@(WorldMap es) =
  assert (isNothing $ w^.objectByPoint p) $
  WorldMap $ case break ((== gid) . _id) es of
              (h, []) -> error "updatePoint: non-existing ID"
              (h, e:t) -> h ++ (e & point ~. p) : t

insertObject :: MapEntity a -> WorldMap a -> WorldMap a
insertObject e (WorldMap es) =
  assert (isNothing $ find (\x -> x^.id == e^.id || x^.point == e^.point)) $
  WorldMap $ e:es

nearbyObjects :: Point -> Radius -> WorldMap a -> [MapEntity a]

empty
