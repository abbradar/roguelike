module Roguelike.ID ( ID
                    , IDGen
                    , next
                    , delete
                    ) where

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default.Generics

newtype ID = ID Integer
           deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype IDGen = IDGen (Set Integer)
              deriving (Show, Eq, FromJSON, ToJSON, Default)

next :: IDGen -> (ID, IDGen)
next (IDGen g)
  | Set.null g = (ID 0, IDGen $ Set.singleton 0)
  | otherwise = let m = succ $ Set.findMax g
                in (ID m, IDGen $ Set.insert m g)

-- Doesn't make any practical sense now; redo so it does
delete :: ID -> IDGen -> IDGen
delete (ID n) (IDGen g) = IDGen $ Set.delete n g
