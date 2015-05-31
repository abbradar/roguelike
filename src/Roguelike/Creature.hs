module Roguelike.Creature where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.TH
import Linear.V2

import Roguelike.Item
import Roguelike.Types
import Roguelike.ID
import Roguelike.Event

type CreatureID = ID

-- | Physical properties of a creature
data Creature = Creature { _visionRadius :: Radius
                         , _hp :: Integer
                         , _inventory :: Map LocalItemID Item
                         , _hand :: Maybe LocalItemID
                         , _appearance :: Appearance
                         }
              deriving (Show)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Creature
makeLenses ''Creature
