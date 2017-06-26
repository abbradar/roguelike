module Roguelike.Action
       ( Action(..)
       , ActionResult(..)
       , TurnStatus(..)
       , finished
       , performAction
       ) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as MB
import Control.Monad.Random
import Control.Lens

import Roguelike.Point
import Roguelike.World
import Roguelike.WorldMap
import Roguelike.Dice
import Roguelike.Creature
import Roguelike.Event
import Roguelike.Item
import Roguelike.Item.MeleeWeapon

data Action = -- Maybe make this relative?
              Move Point
            | Ready (Maybe LocalItemID)
            | Attack LocalCreatureID

data ActionResult = AMovedFrom Point
                  | AReadied Item
                  | AStriked GlobalID Damage
                  | ADropped Item
                  | ASlayed

data TurnStatus = TurnStatus { _finished :: Bool }

makeLenses ''TurnStatus

data PlacedResult = Result GlobalID Point ActionResult

performAction :: MonadRandom m => World -> GlobalID -> TurnStatus -> Action -> m (TurnStatus, [Result])
performAction w id action ts@(TurnStatus True) = skip
performAction w id action ts = case action of
  Move p
    | p == pos -> skip
    | not $ near pos 1 p -> fail "attempt to move further than one square"
    | has (traverse . _2 . _CreatureEntity) (byPoint p $ toWorldMap w) -> fail "square is occupied by a creature already"
    | otherwise -> return ( ts { _finished = True }
                         , [(id, p, AMovedFrom pos)]
                         )
  Ready mid
    | e ^. creature . hand == mid -> skip
    | otherwise -> do
        mitem <- maybe (fail "no such item in inventory") return $ mid >>= (\id -> e ^? creature . inventory . ix id)
        return ( ts { _finished = True }
               , [(id, pos, AReadied mitem)]
               )
  Attack lid -> do
    gid <- maybe (fail "no such person is known to you") return $ e ^? mapping . at lid
    let (gpos, guy) = w ^?! at gid
    unless (near pos 1 gpos) $ fail "enemy is not near you!"
    dmgDice <- maybe (fail "no weapon in hand") return $ e ^? hand . attribute . meleeDamage
    dmg <- roll dmgDice
    let ev = [(id, pos, AStriked gid dmg)]
    return $ if guy ^. hp <= dmg
              -- We can't remove the guy from the world until everyone receives event of his death.
              -- The sequence is:
              -- 1) Place an action result.
              -- 2) Place guys' items on the map.
              -- 3) Notify everyone that the guy is dead and his items appeared.
              -- 4) Remove the guy and the local mappings (and remove him from SubjectiveWorld).
             then let drop = toListOf (inventory . traverse) guy
                  in ( ts { _finished = True }
                     , ev ++ map (\x -> (gid, gpos, ADropped x)) drop ++ [(gid, gpos, ASlayed)]
                     )
             else ( ts { _finished = True }
                  , ev
                  )
    where (pos, CreatureEntity e) = w ^. guy id
          skip = return (w, ts, [])

data MoveResult = MovedOut
                | MovedInto Point
                | MovedInside Point

applyMove :: MonadRandom m => World -> GlobalID -> (GlobalID, MoveResult) -> m World
applyMove w to (from, MovedOut) = a

applyResults :: MonadRandom m => World -> Result -> m World
applyResults w (id, pos, res) = case res of
          AMovedFrom pos' -> -- FIXME: draw a line instead and notify everyone
            let mergeMove MovedFrom (MovedInto p) = MovedInside p
                mergeMove _ _ = error "mergeSight: impossible!"

                toEvent w (id, move) = case move of
                  MovedOut -> w ^. at id . _CreatureEntity . 

                from = M.fromList $ map (second $ const MovedOut) $ detectedBy _visionRadius pos' w
                to = M.fromList $ map (second $ const MovedInto) visible

            in foldM (\w ) w $ M.toList $ M.unionWith mergeMove from to
          AMovedFrom pos' -> foldr update w' visible
          
          where visible = detectedBy _visionRadius pos w'


shapeWill :: MonadRandom m => World -> GlobalID -> TurnStatus -> m (World, TurnStatus)
shapeWill w id ts = do
  let deed = action ts $ w ^?! ix id . _CreatureEntity . ai
  (w', ts', results) <- performAction w id ts deed
  w'' <- foldM applyResults w' results
  return (w'', ts')
