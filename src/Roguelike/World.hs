module Roguelike.World
       ( Entity(..)
       , _CreatureEntity, _ItemEntity
       , WorldCreature
       , creature, ai
       , worldCreature
       , World
       , traverseWorld
       , toWorldMap
       , insert
       , delete
       , detectedBy
       ) where

import Data.Bimap (Map)
import GHC.Generics (Generic)
import Control.Lens
import Data.Default.Generic
import Data.Aeson
import Data.Aeson.TH

import Roguelike.Types
import Roguelike.ID (ID, IDGen)
import qualified Roguelike.ID as I
import Roguelike.Item
import Roguelike.Creature
import Roguelike.AI
import Roguelike.WorldMap (WorldMap)
import qualified Roguelike.WorldMap as W
import Roguelike.SubjectiveWorld

-- We keep global items + creatures namespace.
type GlobalID = ID
type LocalID = ID
data Entity = CreatureEntity WorldCreature
            | ItemEntity Item
            deriving (Generic, FromJSON, ToJSON)

makePrisms ''Entity

-- TODO: Merge items and creatures
--  * Everything has an AI and can produce Actions
--  * Everything can be placed on a map
--  * Everything has a name and appearance (possibly a number of appearance attributes, with their own names)
--  * Something can be taken into inventory
--  * "Item" can produce an Action of increasing bearer's properties, for example
--  * Other actions include spawning a fireball
--  * Fireball is spawned as a result, as a proper Entity with its own AI ("move in a straight line and boom on collision")
data WorldCreature = WorldCreature { _creature :: Creature
                                   -- Allow a human to hijack and control in future
                                   , _ai :: AI
                                   -- "Private" fields
                                   , _mapping :: Bimap GlobalID LocalID
                                   , _localIdGen :: IDGen
                                   }

deriveJSON defaultOptions { fieldLabelModifier = tail } ''WorldCreature
makeLenses ''WorldCreature

worldCreature :: Creature -> AI -> WorldCreature
worldCreature cr ai = WorldCreature { _creature = cr
                                    , _ai = ai
                                    , _mapping = def
                                    , _localIdGen = idGen
                                    }

data World = World { _worldMap :: WorldMap GlobalID Entity
                   , _globalIdGen :: IDGen
                   , _worldItems :: Map 
                   }
             deriving (Generic, Default)

type instance Index World = GlobalID
type instance IxValue World = (Point, Entity)

instance Ixed World where
  ix k = worldMap . ix k

deriveJSON defaultOptions { fieldLabelModifier = tail } ''World
makeLenses ''World

traverseWorld :: IndexedTraversal GlobalID World (Point, Entity)
traverseWorld = worldMap . iptraverse

toWorldMap :: World -> WorldMap GlobalID Entity
toWorldMap = _worldMap

insert :: Point -> Entity -> World -> World
insert p e w = let (id, gen') = I.next $ _globalIdGen w
               in w & globalIdGen .~ gen'
                    & worldMap . at id .~ Just (p, e)

delete :: GlobalID -> World -> World
delete id w = w & worldMap . at id .~ Nothing
                & globalIdGen %~ I.delete id

perceiveWorld :: MonadRandom m => GlobalID -> Event -> World -> m World
perceiveWorld id ev w = a
  where sw = SubjectiveWorld { _personalMap = detectedBy _visionRadius pos' w
                             , _myPosition = w ^. ix id . point
                             }

insertCreature :: MonadRandom m => Point -> Creature -> AI -> World -> m World
insertCreature p cr ai w = do
  let spottedBy = detectedBy _visionRadius p w
      spottedHimself = nearbyObjects (_visionRadius cr) (_worldMap w)
      (id, gen') = I.next $ _globalIdGen w
      wcr = WorldCreature { _creature = cr
                          , _ai = ai
                          , _mapping = def
                          , _localIdGen = idGen
                          }
  in w & globalIdGen .~ gen'
       & worldMap . at id .~ Just (p, EntityCreature wcr)
       & 

applyResult :: MonadRandom m => Result -> World -> m World
applyResult (Result id pos res) w = case res of
  AMovedFrom pos' -> -- FIXME: draw a line instead and notify everyone
    let mergeMove MovedFrom (MovedInto p) = MovedInside p
        mergeMove _ _ = error "mergeSight: impossible!"

        toEvent w (id, move) = case move of
          MovedOut -> w ^. at id . _CreatureEntity . a

        from = M.fromList $ map (second $ const MovedOut) $ detectedBy _visionRadius pos' w
        to = M.fromList $ map (second $ const MovedInto) visible

    in foldM (\w ) w $ M.toList $ M.unionWith mergeMove from to
          
  where visible = detectedBy _visionRadius pos w'

detectedBy :: (Creature -> Radius) -> Point -> Traversal World -> [(GlobalID, WorldCreature)]
-- TODO: SLOW!
detectedBy get p = map (second snd) . filter (\(g, (p', c)) -> near p' (get $ _creature c) p) . toListOf (aside _CreatureEntity . iptraverse . _worldMap)
