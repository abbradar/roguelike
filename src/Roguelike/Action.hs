module Roguelike.Action
       ( MoveAction(..)
       , StandardAction(..)
       , FreeAction(..)
       , Phrase(..)
       , TurnStatus
       , moveCompleted
       , standardCompleted
       , newTurn
       , performAction
       ) where

import Linear
import Control.Lens
import Roguelike.ID
import Roguelike.World
import Roguelike.Dice
import Roguelike.Creature

-- Low-level actions, expected to be executed one-by-one.
-- TODO: All the basic actions, some of advanced ones.
data Action = Aim
            | Move Point
            | Ready Item
            | Reload Item
         -- | Stand
            | Attack AttackAction
         -- | Cast
         -- | Skill
              -- Maybe allow to scream to noone in future.
            | Say EntityID Phrase
            deriving (Show, Eq)

-- Pre-defined phrases (for ease of AI implementing)
-- TODO: Maybe use Strings and parse them, or a small tree...
data Phrase = Follow
            | Stand
            deriving (Show, Read, Eq)

data AttackAction = StandardAttack EntityID
               -- | ChargeAttack Point EntityID
               -- | SwiftAttack EntityID
                  deriving (Show, Eq)

newtype TurnStatus = TurnStatus { _actions :: Int
                                , _attacked :: Bool
                                , _aimed :: Bool
                             -- , _casted :: Bool
                                }
                   deriving (Show, Eq)

makeLenses ''TurnStatus

newTurn :: TurnStatus
newTurn = TurnStatus { _actions = 2
                     , _attacked = False
                     , _aimed = False
                  -- , _casted = False
                     }

advanceTurn :: TurnStatus -> TurnStatus
advanceTurn ts = ts & actions %~ (+2)
                    & attacked .~ False
                    & aimed .~ False

performAction :: MonadRandom m => World -> ID -> TurnStatus -> Action -> m (World, TurnStatus, Map V2 Event])
performAction w e ts action
  | actions ts <= 0 = (w, ts, [])
  | otherwise = case action of
    Aim -> (w, ts & actions -~ 1 & aimed .~ True, [])
    Move p -> let v = p - e^.position
              in if distance v > e^.movementSpeed
                 then fail "attempt to move further than movement speed"
                 else 
