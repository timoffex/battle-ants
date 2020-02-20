module BattleAnts.GUI
    ( showSimulateGame
    ) where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as M
import Graphics.Gloss

import BattleAnts.GameState
import BattleAnts.Graphics.Gloss
import BattleAnts.Simulator
import BattleAnts.World

-- | Shows a simulation of the game.
showSimulateGame :: M.Map PlayerId Color -> GameState -> IO ()
showSimulateGame playerColors game =
  simulate (InWindow "Ant simulation" (800, 600) (0, 0))
           black      -- Background color
           1          -- Simulation steps per second
           game
           (scale 400 400 . renderGame playerColors)
           (\_ _ g -> stepGame g)

-- | Renders the game, given the player colors and the game.
renderGame :: M.Map PlayerId Color -> GameState -> Picture
renderGame playerColors game =
  runReader (renderWorldGrid (game^.world.worldGrid)) $
    mkEnv playerColors game
