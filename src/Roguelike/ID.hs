module Roguelike.ID ( ID
                    , IDGen
                    , next
                    , delete
                    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Default.Generics
import Control.Lens

newtype ID = ID Integer
           deriving (Show, Eq, Ord, FromJSON, ToJSON)

data IDGen = IDGen { _last :: Integer }
           deriving (Show, Eq, Default)

deriveJSON defaultOptions { fieldLabelModifier = tail } ''IDGen
makeLenses ''IDGen

next :: IDGen -> (ID, IDGen)
next (IDGen g) = (g, IDGen $ succ g)

-- Doesn't do anything now; redo so it allows to reuse IDs
delete :: ID -> IDGen -> IDGen
delete _ a = a
