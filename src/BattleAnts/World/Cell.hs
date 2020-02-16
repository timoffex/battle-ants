-- | Defines the data of a world cell.
--
-- This does not define the contents of a cell, e.g. the entities. This is
-- just the cell-specific data.
module BattleAnts.World.Cell
    ( Cell
    , CellData
    , mkCellData
    ) where

import BattleAnts.World.Positioned

data CellData = CellData ()

mkCellData :: CellData
mkCellData = CellData ()

type Cell = WithPosition CellData
