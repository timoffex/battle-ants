{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines what an ant knows about entities.
module BattleAnts.Player.WorldView.EntityView
    ( EntityView
    ) where

import BattleAnts.Player.WorldView.AntView
import BattleAnts.Player.WorldView.ViewedByAnt
import BattleAnts.World

-- | What an ant knows about entities.
data EntityView = EntityAntView AntView

instance ViewedByAnt EntityData EntityView where
  antView v (EntityAnt ad) = EntityAntView $ antView v ad
