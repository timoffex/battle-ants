module BattleAnts.Graphics.Gloss.Cell
    ( renderCell
    ) where

import Control.Monad.Reader
import Graphics.Gloss

import BattleAnts.World

import BattleAnts.Graphics.Gloss.Env


renderCell :: CellData -> Reader Env Picture
renderCell cd = return $ color red $ rectangleWire 1 1
