module Roguelike.Entity where

import Data.Aeson
import Data.Aeson.TH
import Linear.V2

newtype EntityID = EntityID Int
                 deriving (Show, Read, Eq, FromJSON, ToJSON)

type Point = V2 Int

deriving instance FromJSON (V2 a)
deriving instance ToJSON (V2 a)

data Item = Weapon { _range :: Int
                   , _damage :: Dice
                   }
          deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Item
makeLenses ''Item

-- Physical properties of an entity

data Entity = Entity { _position :: Point
                     , _visionRadius :: Int
                     , _speakRadius :: Int
                     , _hearRadius :: Int
                     , _movementSpeed :: Int
                     , _wounds :: Int
                  -- , _attacks :: Int
                     , _fp :: Int
                     , _inventory :: [Item]
                     , _hand :: Maybe Item
                     }
            deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''Entity
makeLenses ''Entity

data EntityAddress = Other EntityID | Myself
                   deriving (Show, Eq)

type Damage = Int

data Event = Appeared EntityAddress Point
           | Disappeared EntityAddress Point
           | Moved EntityAddress Point
           | Striked EntityAddress EntityAddress (Maybe Damage)
           | Slain EntityAddress
           | Heard EntityAddress Phrase
           | Shealthed EntityAddress
           | DrawnOut EntityAddress Item
           deriving (Show, Eq)

updateEntity :: Entity -> Event -> Entity
updateEntity e _ | e ^. wounds <= 0 = error "slain Entity should not get any Events"
updateEntity e (Moved Myself p) = e & position .~ p
updateEntity e (Striked _ Myself (Just dmg)) =
  if res <= 0 && e^.fp > 0
  then e & fp %~ pred & wounds .~ 1
  else e & wounds .~ res

  where res = e^.wounds - dmg
updateEntity e _ = e
