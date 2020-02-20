-- | Defines transition to place new entities into the world.
module BattleAnts.World.Transitions.NewEntities
    ( newEntities
    ) where

import Control.Lens
import Control.Monad.State
import Data.Grid
import qualified Data.Set as S

import BattleAnts.World.Entity
import BattleAnts.World.World
import BattleAnts.World.WorldId

-- | Places new entities into the world, returning their assigned IDs.
--
-- The new entities replace any entities that were in their positions. If
-- there are multiple entities, the later entities override the newer ones.
-- Entities with invalid positions (such as those not in the grid) are not
-- placed (though new IDs are generated for them).
newEntities :: [(GridPosition, EntityData)] -> State World [WorldId]
newEntities entities = do
  allWorldIds <- gets allWorldIdsOf

  let availableIds = filter (flip S.notMember allWorldIds) $ WorldId <$> [1..]

  -- TODO: Add 'nextId' getter to World.
  usedIds <- forM (zip availableIds entities) $ \(wid, (p, e)) -> do
    modify $ set (worldGrid . gridPosition p . entity)
                 (Just $ WithId wid e)
    return wid

  return usedIds
