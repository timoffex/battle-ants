module Main where

import Control.Lens
import Control.Monad.State
import Data.Grid
import qualified Data.Map as M
import Data.SparseGrid
import Graphics.Gloss
import System.Random

import BattleAnts.GameState
import BattleAnts.GUI
import BattleAnts.Player
import BattleAnts.World

main :: IO ()
main = showSimulateGame playerColors game
  where
    playerColors :: M.Map PlayerId Color
    playerColors = M.fromList $ zip allPlayerIds [blue, green]

    game = initialGameState
    allPlayerIds = game ^.. playerMap
                          . itraversed
                          . withIndex
                          . to fst


initialGameState :: GameState
initialGameState = flip evalState (emptyRectWorld (0, 0) (10, 10)) $ do
  p1WorldIds <- newEntities [ (fromXY (0, 0), defaultAnt)
                            , (fromXY (3, 3), defaultAnt) ]
  p2WorldIds <- newEntities [ (fromXY (0, 0), defaultAnt)
                            , (fromXY (3, 3), defaultAnt) ]
  world      <- get

  let playerEntities = [ (player1, p1WorldIds)
                       , (player2, p2WorldIds) ]

  return $ mkGameState world playerEntities (mkStdGen 123)

-- | Creates an empty world.
emptyRectWorld :: (Int, Int) -> (Int, Int) -> World
emptyRectWorld minp maxp = mkWorld $ worldGridFromEntries minp maxp []

-- | Creates a rectangular world grid.
worldGridFromEntries :: (Int, Int)
                    -> (Int, Int)
                    -> [(GridPosition, WorldCellData)]
                    -> SparseGrid WorldCellData
worldGridFromEntries (minX, minY) (maxX, maxY) entries =
  fromEntries $ [ (fromXY (x, y), defaultCell)
                | x <- [minX..maxX], y <- [minY..maxY]
                ] ++ entries

defaultCell :: WorldCellData
defaultCell = mkWorldCellData defaultCellData

defaultCellData :: CellData
defaultCellData = mkCellData

defaultAnt :: EntityData
defaultAnt = EntityAnt $ mkAntData

player1 = mkGamePlayerData dumbAntFunction
player2 = mkGamePlayerData dumbAntFunction

dumbAntFunction :: AntComputation ()
dumbAntFunction = return ()
