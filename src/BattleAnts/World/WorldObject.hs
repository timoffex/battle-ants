{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the 'WorldObject' type, which has an ID and a position.
module BattleAnts.World.WorldObject
    ( WorldObject (WorldObject)
    , idOf
    , HasDatum (datum)
    ) where

import Control.Lens
import Data.Grid

import BattleAnts.World.Positioned
import BattleAnts.World.WorldId

-- | Represents an identifiable object in the world.
--
-- Every object has an ID, a position and its internal data.
data WorldObject d = WorldObject
  { idOf :: WorldId
  , pos :: GridPosition
  , _worldObjectDatum :: d }
makeFields ''WorldObject

instance Positioned (WorldObject d) where
  positionOf = pos
