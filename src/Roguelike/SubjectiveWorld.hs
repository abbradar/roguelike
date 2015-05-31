-- | This is a "current sensory knowledge" of the Creature.
--   For example, current local map of visible creatures are stored here.
--   This is expected to be recreated from the world state anew, without saving.
module Roguelike.SubjectiveWorld where

import Control.Lens
import GHC.Generics (Generic)
import Data.Default.Generics

import Roguelike.Point
import Roguelike.Event
import Roguelike.WorldMap (WorldMap)

data SubjectiveWorld = SubjectiveWorld { _personalMap :: WorldMap LocalID KnownEntity
                                       , _myPosition :: Point
                                       }

makeLenses ''SubjectiveWorld

instance Default SubjectiveWorld where
  def = SubjectiveWorld { _personalMap = def
                        , _myPosition = error "unknown position in SubjectiveWorld"
                        }
