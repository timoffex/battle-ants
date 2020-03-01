-- | Defines transition to place a new entity into the world.
module BattleAnts.World.Transitions.NewEntities
    ( newEntity
    ) where

import Control.Lens
import Control.Monad.State
import Data.Grid
import qualified Data.Set as S

import BattleAnts.World.Entity
import BattleAnts.World.World
import BattleAnts.World.WorldId

-- | Places a entity into the world, returning its assigned ID.
--
-- The new entity replaces any entity at its position. This only fails if the
-- position is not valid in the world.
newEntity :: GridPosition -> EntityData -> StateT World IO (Maybe WorldId)
newEntity p ed = do
  world <- get
  case world ^? worldGrid . gridPosition p of
    Nothing -> return Nothing
    Just _  -> do
      entityId <- lift newWorldId
      modify $ set (worldGrid . gridPosition p . entity)
                   (Just $ WithId entityId ed)
      return $ Just entityId
