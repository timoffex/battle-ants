{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the input to a player's ant function.
module BattleAnts.Player.AntInput
    ( AntInput
    , mkAntInput
    , HasSurroundings (surroundings)
    ) where

import Control.Lens
import Data.Grid3x3
import BattleAnts.Player.WorldView

data AntInput = AntInput
  { _antInputSurroundings :: Grid3x3 (Maybe WorldCellView) }
makeFields ''AntInput

mkAntInput :: Grid3x3 (Maybe WorldCellView) -> AntInput
mkAntInput = AntInput
