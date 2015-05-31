module Roguelike.AI.Class where

import Roguelike.Event
import Roguelike.Creature
import Roguelike.Action
import Roguelike.SubjectiveWorld

instance AI a where
  think :: SubjectiveWorld -> Creature -> a -> Event -> a
  action :: TurnStatus -> a -> Action
