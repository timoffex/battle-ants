module BattleAnts.Actions.ActionResolver
    ( ActionResolver
    ) where

import Control.Monad.State
import Data.Grid
import qualified Data.Map as M
import BattleAnts.World

-- | Resolves actions with associated data of type 'd'.
type ActionResolver d = M.Map GridPosition d -> State World ()
