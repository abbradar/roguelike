module Roguelike.Item.Class where

import Data.Text (Text)

class Attribute a where
  attrDescription :: a -> Maybe Text
