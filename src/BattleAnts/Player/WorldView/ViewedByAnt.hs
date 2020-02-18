{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines the 'ViewedByAnt' class.
module BattleAnts.Player.WorldView.ViewedByAnt
    ( ViewedByAnt (antView)
    ) where

import BattleAnts.World

-- | Class for data that has a corresponding player representation.
class ViewedByAnt a b | a -> b where
  -- | Given an ant, views a value through its perspective.
  antView :: AntData -> a -> b
