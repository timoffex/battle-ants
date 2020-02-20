module BattleAnts.Graphics.Gloss.Ant
    ( renderAnt
    ) where

import Control.Lens
import Control.Monad.Reader
import Data.Maybe
import Graphics.Gloss

import BattleAnts.GameState
import BattleAnts.World

import BattleAnts.Graphics.Gloss.Env


renderAnt :: WithId AntData -> Reader Env Picture
renderAnt a = do
  maybePid <- asks $ view (gameState . antPlayer . at (a^.worldId))

  case maybePid of
    Nothing  -> return blank
    Just pid -> do
      maybeColor <- asks $ view (playerColorMap . at pid)

      let col = fromMaybe magenta maybeColor
      return $ color col $ rectangleSolid 0.8 0.8
