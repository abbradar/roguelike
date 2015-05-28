module Roguelike.Creature where

import Roguelike.ID
import Data.Aeson
import Linear.V2

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
              deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Creature
makeLenses ''Creature

data CreatureAddress = Other ID | Myself
                   deriving (Show, Eq)

type Damage = Int

data Event = Appeared ID Point
           | Disappeared ID Point
           | Moved CreatureAddress Point
           | Striked CreatureAddress CreatureAddress (Maybe Damage)
           | Slain CreatureAddress
           | Heard CreatureAddress Phrase
           | Shealthed CreatureAddress
           | DrawnOut CreatureAddress Item
           deriving (Show, Eq)

updateCreature :: Creature -> Event -> Creature
updateCreature e _ | e ^. wounds <= 0 = error "slain Creature should not get any Events"
updateCreature e (Striked _ Myself (Just dmg)) =
  if res <= 0 && e^.fp > 0
  then e & fp -~ 1 & wounds .~ 1
  else e & wounds .~ res

  where res = e^.wounds - dmg
updateCreature e _ = e
