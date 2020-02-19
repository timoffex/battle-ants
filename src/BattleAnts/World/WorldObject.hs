{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the 'WorldObject' type, which has an ID and a position.
module BattleAnts.World.WorldObject
    ( WorldObject (WorldObject)
    , HasDatum (datum)
    ) where

import Control.Lens
import Data.Grid

import BattleAnts.Fields.HasDatum
import BattleAnts.World.Positioned
import BattleAnts.World.WorldId

-- | Represents an identifiable object in the world.
--
-- Every object has an ID, a position and its internal data.
data WorldObject d = WorldObject
  { _worldObjectWorldId :: WorldId
  , pos                 :: GridPosition
  , _worldObjectDatum   :: d
  } deriving (Functor, Foldable, Traversable)
makeFields ''WorldObject

instance Positioned (WorldObject d) where
  positionOf = pos
