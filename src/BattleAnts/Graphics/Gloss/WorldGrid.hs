module BattleAnts.Graphics.Gloss.WorldGrid
    ( renderWorldGrid
    ) where

import Control.Lens
import Control.Monad.Reader
import Data.Grid
import Data.SparseGrid
import Graphics.Gloss

import BattleAnts.World

import BattleAnts.Graphics.Gloss.WorldCell
import BattleAnts.Graphics.Gloss.Env

renderWorldGrid :: SparseGrid WorldCellData
                -> Reader Env Picture
renderWorldGrid grid = do
  let
      -- When the grid is empty, this pattern fails. However, it
      -- is never invoked in that case anyway, since there are no cells
      -- to transform.
      Just ((minX, minY), (maxX, maxY)) = cornersOf grid

      minXf = fromIntegral minX
      minYf = fromIntegral minY
      maxXf = fromIntegral maxX
      maxYf = fromIntegral maxY

      midXf = (maxXf + minXf) / 2
      midYf = (maxYf + minYf) / 2

      xFactor = 1.0 / (maxXf - minXf)
      yFactor = 1.0 / (maxYf - minYf)

      scaleFactor = min xFactor yFactor

      transformCell :: GridPosition -> Picture -> Picture
      transformCell pos = let (posX, posY) = toXY pos
                              posXf = fromIntegral posX
                              posYf = fromIntegral posY
                          in scale scaleFactor scaleFactor
                             . translate (posXf - midXf) (posYf - midYf)

  cellPics <- forM (withPositions grid) $ \(pos, worldCell) -> do
    cellPic <- renderWorldCell worldCell
    return $ transformCell pos cellPic

  return $ pictures $ cellPics ^.. traverse

-- | Computes the grid's corner points if it is not empty.
--
-- The first pair is the minimum XY coordinates and the second pair is the
-- maximum XY coordinates.
cornersOf :: SparseGrid a -> Maybe ((Int, Int), (Int, Int))
cornersOf grid = foldl minMaxXY Nothing (fst <$> withPositions grid)
  where
    minMaxXY Nothing pos = Just (toXY pos, toXY pos)
    minMaxXY (Just ((minX, minY), (maxX, maxY))) pos =
      let (posX, posY) = toXY pos
      in Just ( (min minX posX, min minY posY)
              , (max maxX posX, max maxY posY) )
