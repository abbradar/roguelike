module Roguelike.State where

import Control.Arrow
import Control.Lens
import Roguelike.Event
import Roguelike.WorldMap (WorldMap)
import qualified Roguelike.WorldMap as W

data WorldState = WorldState { _personalMap :: WorldMap LocalID KnownEntity
                             , _myPosition :: Point
                             }
                deriving (Show)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''WorldState
makeLenses ''WorldState

advanceState :: Event -> WorldState -> WorldState
advanceState ev ws = case ev of
  Appeared id p ent = ws & personalMap . at id .~ Just (p, ent)
  Updated id ent = ws & personalMap . at id %~ fmap (second $ const ent)
  Disappeared id = ws & personalMap . at id .~ Nothing 
  Moved Self p = ws & myPosition .~ p
  _ -> ws
