module Roguelike.AI.Class where

import Control.Monad.Random

import Roguelike.Event
import Roguelike.Creature
import Roguelike.Action
import Roguelike.SubjectiveWorld

class AI a where
  perceive :: MonadRandom m => SubjectiveWorld -> Creature -> a -> Event -> a
  action :: MonadRandom m => TurnStatus -> a -> Action
