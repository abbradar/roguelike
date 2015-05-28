module Roguelike.AI where

import Data.Bimap (Bimap)
import qualified Data.Bimap as B

import Data.Aeson.Dynamic
import Roguelike.AI.Class
import Roguelike.AI.Zombie

newtype AI = AI (SomeObject AI)

instance FromJSON AI where
  parseJSON = parseSomeJSON allAIs

instance ToJSON AI where
  toJSON = toSomeJSON allAIs

allAIs :: Bimap Text (SomeObjectProxy AI)
allAIs = B.fromList [ ("zombie", someP (undefined :: Zombie))
                 -- , ("leader", someP (undefined :: Leader))
                    ]
