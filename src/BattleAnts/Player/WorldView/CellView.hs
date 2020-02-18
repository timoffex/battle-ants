{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines what an ant knows about cells.
module BattleAnts.Player.WorldView.CellView
    ( CellView
    ) where

import BattleAnts.Player.WorldView.ViewedByAnt
import BattleAnts.World

-- | What an ant knows about cells.
data CellView = CellView

instance ViewedByAnt CellData CellView where
  antView _ _ = CellView
