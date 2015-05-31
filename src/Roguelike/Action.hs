module Roguelike.Action
       ( Action(..)
       , 
       , performAction
       ) where

import Control.Lens

import Data.Aeson.Dynamic (someObject)

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

data ActionResult = AMoved
                  | AReadied Item
                  | AStriked GlobalID Damage
                  | ADropped Item
                  | ASlayed

data TurnStatus = TurnStatus { _finished :: Bool }

makeLenses ''TurnStatus

performAction :: MonadRandom m => World -> GlobalID -> TurnStatus -> Action -> m (World, TurnStatus, [(GlobalID, Point. ActionResult)])
performAction w id action ts@(TurnStatus True) = skip
performAction w id action ts = case action of
  Move p
    | p == pos -> skip
    | not $ near pos 1 p -> fail "attempt to move further than one square"
    | has (traverse . _2 . _CreatureEntity) (byPoint p $ toWorldMap w) -> fail "square is occupied by a creature already"
    | otherwise -> return ( w & ix id %~ first (const p)
                         , ts { _finished = True }
                         , [(id, p, AMoved)]
                         )
  Ready mid
    | e ^. creature . hand == mid -> skip
    | otherwise -> do
        mitem <- maybe (fail "no such item in inventory") return $ mid >>= (\id -> e ^? creature . inventory . ix id)
        return ( w & ix id . _2 . _CreatureEntity . creature . hand .~ mid
               , ts { _finished = True }
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
                  in ( foldr (insert gpos . ItemEntity) w drop, 
                     , ts { _finished = True }
                     , ev ++ map (\x -> (gid, gpos, ADropped x)) drop ++ [(gid, gpos, ASlayed)]
                     )
             else ( w & ix gid . _CreatureEntity . creature . hp -~ dmg
                  , ts { _finished = True }
                  , ev
                  )
    where (pos, CreatureEntity e) = w ^. ix id
          skip = return (w, ts, [])

shapeWill :: MonadRandom m => World -> GlobalID -> TurnStatus -> m (World, TurnStatus)
