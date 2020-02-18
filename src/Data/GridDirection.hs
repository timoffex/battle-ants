{-# LANGUAGE NoImplicitPrelude #-}

module Data.GridDirection
    ( GridDirXY (..)
    , GridDirection ( getDirection )
    , NESW (..)
    , shiftToward
    , offsetToward
    ) where

import Prelude (id)
import NumHask.Prelude

import Data.Grid

-- | Class for subsets of 'GridDirXY'.
class GridDirection a where
  getDirection :: a -> GridDirXY

-- | Shifts a 'GridPosition' in the given direction.
shiftToward :: ( GridDirection a )
            => a
            -> GridPosition
            -> GridPosition
shiftToward d p = p + offsetToward d

-- | Returns the 'GridPosition' corresponding to the direction.
offsetToward :: ( GridDirection a )
             => a
             -> GridPosition
offsetToward dir = case getDirection dir of
  NW -> fromXY (-1,  1)
  NN -> fromXY ( 0,  1)
  NE -> fromXY ( 1,  1)
  WW -> fromXY (-1,  0)
  EE -> fromXY ( 1,  0)
  SW -> fromXY (-1, -1)
  SS -> fromXY ( 0, -1)
  SE -> fromXY ( 1, -1)

-- | A direction in the grid.
--
-- The names are to be interpreted in the standard XY coordinate plane where
-- Y is up and X is to the left. North is the positive Y axis, and East is
-- the positive X axis.
data GridDirXY = NW | NN | NE
               | WW      | EE
               | SW | SS | SE
               deriving (Show, Eq)

instance GridDirection GridDirXY where
  getDirection = id


-- | A subset of 'GridDirXY' consisting of 'North', 'East', 'West', 'South'.
--
-- 'North' is 'NN', 'West' is 'WW', 'East' is 'EE' and 'South' is 'SS'.
data NESW =      North
          | West      | East
               | South
          deriving (Show, Eq)

instance GridDirection NESW where
  getDirection North = NN
  getDirection East  = EE
  getDirection South = SS
  getDirection West  = WW
