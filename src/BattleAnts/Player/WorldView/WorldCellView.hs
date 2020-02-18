{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines what an ant knows about entities.
module BattleAnts.Player.WorldView.WorldCellView
    ( WorldCellView
    , HasCellView (cellView)
    , HasEntityView (entityView)
    ) where

import Control.Lens

import BattleAnts.Player.WorldView.CellView
import BattleAnts.Player.WorldView.EntityView
import BattleAnts.Player.WorldView.ViewedByAnt
import BattleAnts.World

-- | What an ant knows about entities.
data WorldCellView = WorldCellView
  { _worldCellViewCellView :: CellView
  , _worldCellViewEntityView :: (Maybe (WithId EntityView))
  }
makeFields ''WorldCellView

instance ViewedByAnt WorldCellData WorldCellView where
  antView v wc = WorldCellView (antView v $ wc ^. cell)
                               (antView v $ wc ^. entity)
