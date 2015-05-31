module Roguelike.Point where

import Data.Aeson
import Linear

import Roguelike.Types

type Point = V2 Integer
type Vect = Point

deriving instance FromJSON (V2 a)
deriving instance ToJSON (V2 a)

distanceP :: Point -> Point -> Distance
distanceP a b = floor $ norm (toF <$> a - b)
  where toF :: Integral a => a -> Double
        toF = fromIntegral

near :: Point -> Radius -> Point -> Bool
near a r b = (<= r) . distanceP a b
