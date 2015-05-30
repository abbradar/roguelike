module Roguelike.AI.Class where

import Roguelike.Event
import Roguelike.Creature
import Roguelike.Action
import Roguelike.SubjectiveWorld

instance AI a where
  think :: SubjectiveWorld -> Creature -> TurnStatus -> a -> Event -> ([Action], a)
