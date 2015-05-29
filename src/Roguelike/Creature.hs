module Roguelike.Creature where

import Data.Aeson
import Linear.V2

import Roguelike.ID
import Roguelike.Event

-- Physical properties of a creature n entity

type Radius = Int

data Creature = Creature { _visionRadius :: Radius
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
  if res <= 0 && e^.fp > 0
  then e & fp -~ 1 & wounds .~ 1
  else e & wounds .~ res

  where res = e^.wounds - dmg
updateCreature e _ = e
