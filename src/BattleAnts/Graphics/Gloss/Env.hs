{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the environment shared by all graphics functions for Gloss.
module BattleAnts.Graphics.Gloss.Env
    ( Env
    , HasPlayerColorMap (playerColorMap)
    ) where

import Control.Lens
import Graphics.Gloss
import qualified Data.Map as M

import BattleAnts.GameState

data Env = Env
  { _envPlayerColorMap :: M.Map PlayerId Color
  }
makeFields ''Env

mkEnv :: M.Map PlayerId Color -> Env
mkEnv = Env
