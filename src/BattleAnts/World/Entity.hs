{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the concept of an 'Entity' in the world.
module BattleAnts.World.Entity
    (
    ) where

import Control.Lens

import BattleAnts.World.Ant
import BattleAnts.World.WorldObject

data EntityData = EntityAnt AntData
makePrisms ''EntityData

type Entity = WorldObject EntityData

-- | Class for subsets of 'EntityData'.
--
-- The name 'fromEntityData' is meant to compose well with lenses, e.g.
--
-- @
--   entityData . fromEntityData
--        :: Prism' Cell AntData
--        :: Prism' Cell FoodData
--        :: ...
-- @
class IsEntityData d where
  fromEntityData :: Prism' EntityData d

instance IsEntityData AntData where
  fromEntityData = _EntityAnt
