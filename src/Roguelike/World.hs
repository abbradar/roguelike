module Roguelike.World where

import Linear
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.TH
import Roguelike.Point
import Roguelike.Creature
import Roguelike.Item
import Roguelike.AI
import Roguelike.ID

data Entity = CreatureEntity { _creature :: Creature
                             , _mapping :: Map GlobalID LocalID
                             , _idGen :: IDGen
                             }
            deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Entity
makeLenses ''Entity

-- TODO: more effective structure
data World = World { _objects : [Entity]
                   , _idGen :: IDGen
                   }
              deriving (Show)
