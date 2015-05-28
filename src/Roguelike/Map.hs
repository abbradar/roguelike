module Roguelike.Map ( GlobalID(..)
                     , LocalID(..)
                     , Entity(..)
                     , MapEntity(..)
                     , WorldMap
                     , entityById
                     , entityByPoint
                     , mentityById
                     , mentityByPoint
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.TH
import Control.Lens
import Roguelike.ID
import Roguelike.Creature

newtype GlobalID = GlobalID ID
                 deriving (Show, Eq)
-- TODO: implement
newtype LocalID = LocalID ID
                deriving (Show, Eq)

data Object a = Entity a
              | Item a
              deriving (Show, Eq)

-- Most common world map operations would be:
-- 1) Search by ID
-- 2) Search all entities near a Point
-- TODO: More effectiveness!

data MapEntity a = MapEntity { _id :: !Object GlobalID
                             , _point :: !Point
                             , _entity :: a
                             }
                 deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''MapEntity
makeLenses ''MapEntity

type MapKey = (GlobalID, Point)

mapKey :: MapEntity a -> MapKey
mapKey e = (e^.id, e^.point)

newtype WorldMap a = WorldMap [MapEntity a]
                 deriving (Show, Read, Eq, FromJSON, ToJSON)

instance FunctorWithIndex MapKey WorldMap where
  imap f (WorldMap es) = WorldMap $ map (\e -> e & entity %~ f (mapKey e)) es

instance Functor WorldMap where
  fmap = imap . const

instance FoldableWithIndex MapKey WorldMap where
  ifoldMap f (WorldMap es) = foldMap (\e -> f (mapKey e) $ e^.entity) es

instance Foldable WorldMap where
  foldMap = ifoldMap . const

instance TraversableWithIndex MapKey WorldMap where
  itraverse f (WorldMap es) = WorldMap <$> traverse (\e -> fmap (flip (entity .~) e) $ f (mapKey e) $ e^.entity) es

instance Traversable WorldMap where
  traverse = itraverse . const

mentityBySmth :: (MapEntity a -> b) -> b -> Lens' (WorldMap a) (Maybe (MapEntity a))
-- (<&>) = flip (<$>) -- from Control.Lens.Operators
mentityBySmth get k f (WorldMap es) = f r <&> \x -> case (x, r) of
  (Nothing, _) -> WorldMap $ h ++ tail t
  (Just v, Just _) -> WorldMap $ h ++ v : tail t
  (Just _, Nothing) -> error "entityBySmth: cannot add entity by index"
    where (h, t) = break ((== k) . get) es
          r = listToMaybe t

entityById :: GlobalID -> Lens' (WorldMap a) (Maybe a)
entityById = entityBySmth _id

entityByPoint :: Point -> WorldMap a -> Maybe a
entityByPoint =entityBySmth _point

mentityById :: GlobalID -> Getter (WorldMap a) (Maybe (MapEntity a))
mentityById = mentityBySmth _id

mentityByPoint :: GlobalID -> Getter (WorldMap a) (Maybe (MapEntity a))
mentityByPoint = mentityBySmth _point

moveEntity :: GlobalID -> Point -> WorldMap a -> WorldMap a
moveEntity gid p w@(WorldMap es) =
  assert (isNothing $ w^.entityByPoint p) $
  WorldMap $ case break ((== gid) . _id) es of
              (h, []) -> error "updatePoint: non-existing ID"
              (h, e:t) -> h ++ (e & point ~. p) : t

insertEntity :: MapEntity a -> WorldMap a -> WorldMap a
insertEntity e (WorldMap es) =
  assert (isNothing $ find (\x -> x^.id == e^.id || x^.point == e^.point)) $
  WorldMap $ e:es

nearbyEntities :: Point -> Radius -> WorldMap a -> [MapEntity a]
