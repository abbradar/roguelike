module Roguelike.Item.MeleeWeapon where

import Data.Typeable
import Data.Aeson
import Control.Lens

import Roguelike.Item.Class
import Roguelike.Dice

data MeleeWeapon = MeleeWeapon { _meleeDamage :: Dice }
                 deriving (Show, Read, Typeable)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''MeleeWeapon
makeLenses ''MeleeWeapon

instance Attribute MeleeWeapon where
  attrDescription _ = Nothing
