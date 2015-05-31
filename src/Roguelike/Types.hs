module Roguelike.Types where

import Data.Default.Generics
import Data.Bimap (Bimap)
import qualified Data.Bimap as MB

type Distance = Int
type Radius = Distance

instance Default (Bimap k a) where
  def = MB.null
