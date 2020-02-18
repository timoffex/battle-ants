-- | Defines the movement action and resolver.
module BattleAnts.Actions.MoveAction
    ( MoveData ( MoveData )
    , resolveMoveActions
    ) where

import Data.GridDirection
import qualified Data.Map as M

import BattleAnts.Actions.ActionResolver
import BattleAnts.World

-- | The data defining a move action.
data MoveData = MoveData NESW

-- | Resolves the move action.
resolveMoveActions :: ActionResolver MoveData
resolveMoveActions moveActionMap = moveEntities moves >> return ()
  where
    moves = M.mapKeys Origin $ M.mapWithKey toDestination moveActionMap

    toDestination pos (MoveData dir) = Destination $ shiftToward dir pos
