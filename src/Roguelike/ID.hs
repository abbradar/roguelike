module Roguelike.ID ( ID
                    , IDGen
                    , idGen
                    , nextId
                    , removeId
                    ) where

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as Set

newtype ID = ID Integer
           deriving (Show, Eq, FromJSON, ToJSON)

newtype IDGen = IDGen (Set Integer)
              deriving (Show, Eq, FromJSON, ToJSON)

idGen :: IDGen
idGen = IDGen Set.empty

nextId :: IDGen -> (ID, IDGen)
nextId (IDGen g)
  | Set.null g = (ID 0, IDGen $ Set.singleton 0)
  | otherwise = let m = succ $ Set.findMax g
                in (ID m, IDGen $ Set.insert m g)

removeId :: ID -> IDGen -> IDGen
removeId (ID n) (IDGen g) = IDGen $ Set.delete n g
