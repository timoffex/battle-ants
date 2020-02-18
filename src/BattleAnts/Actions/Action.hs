{-# LANGUAGE TemplateHaskell #-}

-- | Defines the action types and how to resolve them.
module BattleAnts.Actions.Action
    ( resolveActions
    , ActionData ( ActionMove )
    , _ActionMove
    ) where

import Control.Lens
import Control.Monad.ST
import Control.Monad.State
import qualified Data.Map as M
import Data.STRef

import BattleAnts.Actions.ActionResolver
import BattleAnts.Actions.MoveAction

-- | Defines an action.
data ActionData = ActionMove MoveData
makePrisms ''ActionData

-- | Resolves actions.
resolveActions :: ActionResolver ActionData
resolveActions actionMap = do
  let extract p = M.fromList $ M.assocs actionMap ^.. traverse . aside p

      moveActions = extract _ActionMove

  resolveMoveActions moveActions
