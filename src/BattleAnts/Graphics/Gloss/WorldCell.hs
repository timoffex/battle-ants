module BattleAnts.Graphics.Gloss.WorldCell
    ( renderWorldCell
    ) where

import Control.Lens
import Control.Monad.Reader
import Graphics.Gloss

import BattleAnts.World

import BattleAnts.Graphics.Gloss.Cell
import BattleAnts.Graphics.Gloss.Entity
import BattleAnts.Graphics.Gloss.Env

renderWorldCell :: WorldCellData
                -> Reader Env Picture
renderWorldCell wcd = do
  cellPicture   <- renderCell $ wcd^.cell
  entityPicture <- case wcd^.entity of
    Nothing -> return blank
    Just e  -> renderEntity e

  return $ pictures [cellPicture, entityPicture]
