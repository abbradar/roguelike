module Roguelike.Event where

import Roguelike.ID
import Roguelike.Action
import Roguelike.Item

newtype LocalID = LocalID ID
                deriving (Show, Eq)

data CreatureAddress = Other LocalID | Myself
                   deriving (Show, Eq)

type Damage = Int

-- Pretty useless for now
data KnownEntity = KnownCreature
                 | KnownItem Item
                 deriving (Show, FromJSON, ToJSON)

data Event = Appeared LocalID Point KnownEntity
           | Updated LocalID KnownEntity
           | Disappeared LocalID Point
           | Moved CreatureAddress Point
           | Striked CreatureAddress CreatureAddress (Maybe Damage)
           | Slain CreatureAddress
           | Heard CreatureAddress Phrase
           | Shealthed CreatureAddress
           | DrawnOut CreatureAddress Item
           deriving (Show)
