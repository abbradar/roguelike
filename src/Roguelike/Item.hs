module Roguelike.Item
       ( Attributes
       , Item(..)
       , itemName
       , attributes
       ) where

import Data.Text (Text)
import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import Data.Aeson
import Data.Aeson.TH
import Control.Lens

import Data.Aeson.Dynamic
import Data.IndexedSet (IndexedSet)
import qualified Data.IndexedSet as I

import Roguelike.Item.Class
import Roguelike.Item.MeleeWeapon

type Attributes = IndexedSet TypeRep (SomeObject Attribute)

data Item = Item { _itemName :: Text
                 , _attributes :: Attributes
                 }
          deriving (Show, Read)

makeLenses ''Item

instance ToJSON Item where
  toJSON item = object [ "name" :. _itemName item
                       , "attributes" :. toSomesJSON allAttributes (_attributes item)
                       ]

instance FromJSON Item where
  parseJSON item (Object obj) = Object <$>
                                obj .: "name" <*>
                                (obj .: "attributes" >>= parseSomesJSON allAttributes)

allAttributes :: Bimap Text (SomeObjectProxy Attribute)
allAttributes = B.fromList [ ("melee_weapon", someP (undefined :: MeleeWeapon))
                           ]
