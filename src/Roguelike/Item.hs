module Roguelike.Item
       ( Attributes
       , Item(..)
       , attribute
       , itemName
       , attributes
       ) where

import Data.Text (Text)
import Data.Proxy
import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import Data.Aeson
import Data.Aeson.TH
import Control.Lens

import Data.Aeson.Dynamic
import Data.IndexedSet (IndexedSet)
import qualified Data.IndexedSet as I

import Roguelike.ID
import Roguelike.Item.Class
import Roguelike.Item.MeleeWeapon

type Attributes = IndexedSet TypeRep (SomeObject Attribute)

type ItemID = ID

data Item = Item { _itemName :: Text
                 , _attributes :: Attributes
                 }
          deriving (Show, Read)

makeLenses ''Item

attribute :: forall a. (Attribute a, SomeObjectable a) => Prism' Item a
attribute = attributes . ix (typeRep (Proxy :: Proxy a)) . someObject

instance ToJSON Item where
  toJSON item = object [ "name" :. _itemName item
                       , "attributes" :. toSomesJSON allAttributes (_attributes item)
                       ]

instance FromJSON Item where
  parseJSON item (Object obj) = Object <$>
                                obj .: "itemName" <*>
                                (obj .: "attributes" >>= parseSomesJSON allAttributes)

allAttributes :: Bimap Text (SomeObjectProxy Attribute)
allAttributes = B.fromList [ ("MeleeWeapon", someP (undefined :: MeleeWeapon))
                           ]
