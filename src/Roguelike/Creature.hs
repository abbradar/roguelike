module Roguelike.Creature where

import Data.Aeson
import Linear.V2

import Roguelike.Types
import Roguelike.ID
import Roguelike.Event

-- Physical properties of a creature

data Creature = Creature { _position :: Point
                         , _visionRadius :: Radius
                         , _speakRadius :: Radius
                         , _hearRadius :: Radius
                         , _movementSpeed :: Radius
                         , _wounds :: Int
                      -- , _attacks :: Int
                         , _fp :: Int
                         , _inventory :: [Item]
                         , _hand :: Maybe Item
                         }
              deriving (Show)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Creature
makeLenses ''Creature

updateCreature :: Creature -> Event -> Creature
updateCreature e _ | e ^. wounds <= 0 = error "slain Creature should not get any Events"
updateCreature e (Striked _ Myself (Just dmg)) =
  let res = e^.wounds - dmg
  in if res <= 0 && e^.fp > 0
     then e & fp -~ 1 & wounds .~ 1
     else e & wounds .~ res
updateCreature e (Moved Myself p) = e & position .~ p
updateCreature e _ = e
