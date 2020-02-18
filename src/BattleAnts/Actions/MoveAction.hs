-- | Defines the movement action and resolver.
module BattleAnts.Actions.MoveAction
    ( MoveData ( MoveData )
    ) where

import Data.GridDirection
import qualified Data.Map as M

import BattleAnts.Actions.ActionResolver
import BattleAnts.World

-- | The data defining a move action.
data MoveData = MoveData NESW

-- | Resolves the move action.
moveActionResolver :: ActionResolver MoveData
moveActionResolver moveActionMap = moveEntities moves >> return ()
  where
    moves = M.mapKeys Origin $ M.mapWithKey toDestination moveActionMap

    toDestination pos (MoveData dir) = Destination $ shiftToward dir pos
