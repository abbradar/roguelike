module Roguelike.Types where

import Data.Aeson
import Linear

type Point = V2 Int
type Vect = Point

deriving instance FromJSON (V2 a)
deriving instance ToJSON (V2 a)

type Distance = Int
type Radius = Distance

distanceP :: Point -> Point -> Distance
distanceP a b = floor <$> distance (toF <$> a) (toF <$> b)
  where toF :: Integral a => a -> Float
        toF = fromIntegral
