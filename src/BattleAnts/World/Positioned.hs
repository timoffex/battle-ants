{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the 'Positioned' class for data with positions.
module BattleAnts.World.Positioned
    ( Positioned (positionOf)
    , WithPosition (WithPosition)
    , HasPosition (position)
    , HasDatum (datum)
    ) where

import Control.Lens
import Data.Grid

import BattleAnts.Fields.HasDatum

-- | Class for data with a readable position.
class Positioned a where
  positionOf :: a -> GridPosition

-- | Adds a position to a datum.
data WithPosition d = WithPosition
  { _withPositionPosition :: GridPosition
  , _withPositionDatum :: d
  }
makeFields ''WithPosition

instance Positioned (WithPosition d) where
  positionOf = view position
