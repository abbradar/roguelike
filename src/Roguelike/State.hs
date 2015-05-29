module Roguelike.State where

import Roguelike.Event
import Roguelike.WorldMap (WorldMap)
import qualified Roguelike.WorldMap as W

data WorldState = WorldState { _personalMap :: WorldMap LocalID KnownEntity }
                deriving (Show)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''WorldState
makeLenses ''WorldState

emptyState :: WorldState
emptyState = WorldState { _personalMap = W.empty }

data Event = Appeared LocalID Point KnownEntity
           | Updated LocalID KnownEntity
           | Disappeared LocalID Point
           | Moved CreatureAddress Point
           | Striked CreatureAddress CreatureAddress (Maybe Damage)
           | Slain CreatureAddress
           | Heard CreatureAddress Phrase
           | Shealthed CreatureAddress
           | DrawnOut CreatureAddress Item

advanceState :: Event -> WorldState -> WorldState
advanceState ev ws = case ev of
  Appeared id p ent = ws & personalMap %~ 
  Updated id ent = ws 
