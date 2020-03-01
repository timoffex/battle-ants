{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the full state of the game.
module BattleAnts.GameState
    ( GameState
    , mkGameState
    , HasWorld (world)
    , HasPlayerMap (playerMap)
    , HasAntPlayer (antPlayer)
    , HasRandomSeed (randomSeed)

    , playerById
    , playerDataFor

    , PlayerId
    , GamePlayerData
    , mkGamePlayerData
    , HasAntFunction (antFunction)
    ) where

import Control.Lens
import Control.LensUtils
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Utils
import Data.Grid
import Data.Grid3x3
import qualified Data.Map as M
import Data.Unique
import System.Random

import BattleAnts.Player
import BattleAnts.World

newtype PlayerId = PlayerId Unique
                   deriving (Eq, Ord)

newPlayerId :: IO PlayerId
newPlayerId = PlayerId <$> newUnique

-- | What defines a player, save for the ID.
data GamePlayerData = GamePlayerData
  { _gamePlayerDataAntFunction :: AntComputation () }
makeFields ''GamePlayerData

mkGamePlayerData :: AntComputation () -> GamePlayerData
mkGamePlayerData = GamePlayerData

-- | The full state of the game.
data GameState = GameState
  { _gameStateWorld       :: World
  , _gameStatePlayerMap   :: M.Map PlayerId GamePlayerData
  , _gameStateAntPlayer   :: M.Map WorldId PlayerId
  , _gameStateRandomSeed  :: StdGen
  }
makeFields ''GameState

-- | Initializes a game state given an association between players and entities.
mkGameState :: World
            -> [(GamePlayerData, [WorldId])]
            -- ^ Players and the IDs of their associated entities.
            -> StdGen
            -- ^ Initial random seed.
            -> IO GameState
mkGameState world playerDefs seed = do

  idsPlayerDefs <- forM playerDefs $ \pd -> do
    playerId <- newPlayerId
    return (playerId, pd)

  let playerMap     = M.fromList $ [ (i, p)  | (i, (p, _ )) <- idsPlayerDefs ]
      antPlayerMap  = M.fromList $ [ (wi, i) | (i, (_, ws)) <- idsPlayerDefs
                                            , wi <- ws ]

  return $ GameState world playerMap antPlayerMap seed


-- | Gets the data of the player by ID.
playerById :: PlayerId -> GameState -> Maybe GamePlayerData
playerById pid gs = gs ^. playerMap . at pid

-- | Gets the data of the player associated to the entity.
playerDataFor :: WorldId -> GameState -> Maybe GamePlayerData
playerDataFor wid gs = do
  pid <- gs ^. antPlayer . at wid
  playerById pid gs
