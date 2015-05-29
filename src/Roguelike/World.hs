module Roguelike.World
       ( 
       ) where

import Linear
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.TH

import Roguelike.Creature
import Roguelike.Item
import Roguelike.AI
import Roguelike.ID

-- We keep global items + creatures namespace.
newtype GlobalID = GlobalID ID
                 deriving (Show, Eq)

data Entity = CreatureEntity WorldCreature
            | ItemEntity Item
            deriving (Show, Eq, FromJSON, ToJSON)

-- Most common world map operations would be:
-- 1) Search by ID
-- 2) Search all entities near a Point
-- TODO: More effectiveness!

data WorldCreature = WorldCreature { _creature :: Creature
                                   , _mapping :: Map GlobalID LocalID
                                   , _localIdGen :: IDGen
                                     -- Allow a human to hijack and control in future
                                   , _ai :: AI
                                   }
            deriving (Show, Eq)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''WorldCreature
makeLenses ''WorldCreature

-- TODO: more effective structure
data World = World { _worldMap :: WorldMap GlobalID Entity
                   , _globalIdGen :: IDGen
                   }
              deriving (Show)
