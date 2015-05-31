module Roguelike.World
       ( Entity(..)
       , _CreatureEntity, _ItemEntity
       , WorldCreature
       , creature, ai
       , worldCreature
       , World
       , traverseWorld
       , toWorldMap
       , insert
       , delete
       ) where

import Data.Bimap (Bimap)
import GHC.Generics (Generic)
import Control.Lens
import Data.Default.Generic
import Data.Aeson
import Data.Aeson.TH

import Roguelike.Types
import Roguelike.ID (ID, IDGen)
import qualified Roguelike.ID as I
import Roguelike.Item
import Roguelike.Creature
import Roguelike.AI
import Roguelike.WorldMap (WorldMap)
import qualified Roguelike.WorldMap as W
import Roguelike.SubjectiveWorld

-- We keep global items + creatures namespace.
type GlobalID = ID
type LocalID = ID
data Entity = CreatureEntity WorldCreature
            | ItemEntity Item
            deriving (Generic, FromJSON, ToJSON)

makePrisms ''Entity

data WorldCreature = WorldCreature { _creature :: Creature
                                   -- Allow a human to hijack and control in future
                                   , _ai :: AI
                                   -- "Private" fields
                                   , _mapping :: Bimap GlobalID LocalID
                                   , _localIdGen :: IDGen
                                   , _subjectiveWorld :: SubjectiveWorld
                                   }

makeLenses ''WorldCreature

worldCreature :: Point -> Creature -> AI -> WorldCreature
worldCreature p cr ai = WorldCreature { _creature = cr
                                      , _ai = ai
                                      , _mapping = def
                                      , _localIdGen = idGen
                                      , _subjectiveWorld = def
                                      }

instance FromJSON WorldCreature where
  parseJSON (Object obj) = do
    _creature <- obj .: "creature"
    _ai <- obj .: "ai"
    _mapping <- obj .: "mapping"
    _localIdGen <- obj .: "localIdGen"
    let _subjectiveWorld = def
    return WorldCreature {..}

instance ToJSON WorldCreature where
  toJSON (WorldCreature {..}) = object [ "creature" .= _creature
                                       , "ai" .= _ai
                                       , "mapping" .= _mapping
                                       , "localIdGen" .= _localIdGen
                                       ]

data World = World { _worldMap :: WorldMap GlobalID Entity
                   , _globalIdGen :: IDGen
                   }
             deriving (Generic, Default)

type instance Index World = GlobalID
type instance IxValue World = (Point, Entity)

instance Ixed World where
  ix k = worldMap . ix k

deriveJSON defaultOptions { fieldLabelModifier = tail } ''World
makeLenses ''World

traverseWorld :: IndexedTraversal GlobalID World (Point, Entity)
traverseWorld = worldMap . iptraverse

toWorldMap :: World -> WorldMap GlobalID Entity
toWorldMap = _worldMap

insert :: Point -> Entity -> World -> World
insert p e w = let (id, gen') = I.next $ _globalIdGen w
               in w & globalIdGen .~ gen'
                    & worldMap . at id .~ Just (p, e)

delete :: GlobalID -> World -> World
delete id w = w & worldMap . at id .~ Nothing
                & globalIdGen %~ I.delete id
