{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the output type for a player's ant function.
module BattleAnts.Player.AntOutput
    ( AntOutput
    , mkAntOutput
    , HasAction (action)
    ) where

import Control.Lens
import BattleAnts.Actions

data AntOutput = AntOutput
  { _antOutputAction :: Maybe ActionData }
makeFields ''AntOutput

mkAntOutput :: AntOutput
mkAntOutput = AntOutput Nothing
