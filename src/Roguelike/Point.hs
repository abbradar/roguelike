module Roguelike.Point where

import Linear

type Point = V2 Int

deriving instance FromJSON (V2 a)
deriving instance ToJSON (V2 a)
