module BattleAnts.Graphics.Gloss.Entity
    ( renderEntity
    ) where

import Control.Lens
import Control.Monad.Reader
import Graphics.Gloss

import BattleAnts.World

import BattleAnts.Graphics.Gloss.Ant
import BattleAnts.Graphics.Gloss.Env

renderEntity :: WithId EntityData -> Reader Env Picture
renderEntity e =
  case e^.datum of
    EntityAnt ad -> renderAnt $ ad <$ e
