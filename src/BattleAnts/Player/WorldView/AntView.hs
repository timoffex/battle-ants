{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines what an ant knows about ants.
module BattleAnts.Player.WorldView.AntView
    ( AntView
    ) where

import BattleAnts.Player.WorldView.ViewedByAnt
import BattleAnts.World

-- | What an ant knows about ants.
data AntView = AntView

instance ViewedByAnt AntData AntView where
  antView _ _ = AntView
