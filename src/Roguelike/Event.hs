module Roguelike.Event where

import Data.Aeson

import Roguelike.ID
import Roguelike.Action
import Roguelike.Item

data CreatureAddress = Other CreatureID | Myself
                     deriving (Show, Eq)

type Damage = Int

-- Something like a global ID for now
newtype Appearance = Appearance ID
                   deriving (Show, Eq, Ord, FromJSON, ToJSON)

data KnownEntity = KnownCreature Appearance
                 | KnownItem Item
                 deriving (Show)

-- TODO: Maybe send only relative movements; let's conceal the real location!
data Event = Appeared ID Point KnownEntity
           | Updated ID KnownEntity
           | Disappeared ID
           | Moved CreatureAddress Point
           | Striked CreatureAddress CreatureAddress (Maybe Damage)
           | Slain CreatureAddress
           deriving (Show)
