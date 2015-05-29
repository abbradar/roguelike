module Roguelike.AI.Class where

import Roguelike.Event
import Roguelike.Entity

instance AI a where
  think :: Entity -> TurnStatus -> a -> Event -> ([Action], a)
