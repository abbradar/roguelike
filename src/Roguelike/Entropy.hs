module Roguelike.Entropy where

import Data.Monoid

type Tick = Integer

-- TODO: how do I even name this thing?
data Entropied a = Latest a
                 | Stamped Tick a
                 | Unknown
                 deriving (Show, Eq)

-- (shoe) shu

instance Functor Entropied where
  fmap f (Latest a) = Latest (f a)
  fmap f (Stamped t a) = Stamped t (f a)
  fmap f Unknown = Unknown

-- It isn't a valid Alternative, because it isn't an Applicative!

instance Monoid (Entropied a) where
  mempty = Unknown

  a `mappend` Unknown = a
  (Latest a) `mappend` (Stamped t b) = Latest a
  (Stamped t1 a) `mappend` (Stamped t2 b) | t1 > t2 = Stamped t1 a
  _ `mappend` b = b

class ValueEq a where
  match :: a -> a -> Bool

instance Eq a => ValueEq (Entropied a) where
  match Unknown _ = True
  -- TODO: Booth!
  match _ Unknown = True
  match Unknown _ = True
  match (Stamped _ a) (Stamped _ b) | a == b = True
  match (Stamped _ a) (Latest b) | a == b = True
  match (Latest a) (Stamped _ b) | a == b = True
  match (Latest a) (Latest b) | a == b = True
  match _ _ = False

data Concealed a = Concealed a (Entropied a)
                 deriving (Show, Eq)

instance Eq a => ValueEq (Concealed a) where
  match (Concealed a1 b1) (Concealed a2 b2) = a1 == a2 && match b1 b2
