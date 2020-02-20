module BattleAnts.Simulator
    ( stepGame
    ) where

import Control.Lens
import Control.LensUtils
import Control.Utils
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Grid
import Data.Grid3x3
import qualified Data.Map as M
import System.Random

import BattleAnts.Actions
import BattleAnts.Player
import BattleAnts.GameState
import BattleAnts.World

-- | Advances the game by one step.
stepGame :: GameState -> GameState
stepGame game = game & world      %~ updateWorld
                     & randomSeed .~ stdGen'
  where
    (antOutputs, stdGen') = flip runState (game^.randomSeed) $ runAnts game

    actionMap = M.fromList $ do
      ao <- antOutputs
      case ao^.datum.action of
        Just a -> return (positionOf ao, a)
        Nothing -> []

    updateWorld = execState $ resolveActions actionMap


-- | Runs the ant if it has an associated player.
runAntIn :: GameState -> Ant -> MaybeT (State StdGen) AntOutput
runAntIn game ant = do
  playerData  <- liftMaybe $ playerDataFor (ant^.worldId) game
  let antInput = mkAntInput $ antView (ant^.datum) $ center3x3 $
                   game ^. world . worldGrid . centeredAt (positionOf ant)
  (_, ao)     <- lift $ runAntComputation antInput (playerData ^. antFunction)
  return ao

-- | Gets the outputs of all ants.
runAnts :: GameState -> State StdGen [WithPosition AntOutput]
runAnts game = do
  let ants = game ^... world . traverseAnts

  antOutputs <- forM ants $ \ant -> do
    maybeAntOutput <- runMaybeT (runAntIn game ant)
    case maybeAntOutput of
      Just ao -> return [WithPosition (positionOf ant) ao]
      Nothing -> return []

  return $ join antOutputs
