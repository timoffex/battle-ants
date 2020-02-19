{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the full state of the game.
module BattleAnts.GameState
    ( GameState
    , mkGameState
    , HasWorld (world)

    , runAnts
    , runAntIn
    , playerById
    , playerDataFor

    , PlayerId
    , GamePlayerData
    , mkGamePlayerData
    , HasAntFunction (antFunction)
    ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Utils
import Data.Functor.Const
import Data.Grid
import Data.Grid3x3
import qualified Data.Map as M
import Data.Monoid.Endo
import System.Random

import BattleAnts.Player
import BattleAnts.World

newtype PlayerId = PlayerId Int
                   deriving (Eq, Ord)

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
  }
makeFields ''GameState

-- | Initializes a game state given an association between players and entities.
mkGameState :: World
            -> [(GamePlayerData, [WorldId])]
            -- ^ Players and the IDs of their associated entities.
            -> GameState
mkGameState world playerDefs = GameState world playerMap antPlayerMap
  where
    idsPlayerDefs = zip (map PlayerId [1..]) playerDefs
    playerMap    = M.fromList $ [ (i, p)  | (i, (p, _ )) <- idsPlayerDefs ]
    antPlayerMap = M.fromList $ [ (wi, i) | (i, (_, ws)) <- idsPlayerDefs
                                          , wi <- ws ]

-- | Gets the data of the player by ID.
playerById :: PlayerId -> GameState -> Maybe GamePlayerData
playerById pid gs = gs ^. playerMap . at pid

-- | Gets the data of the player associated to the entity.
playerDataFor :: WorldId -> GameState -> Maybe GamePlayerData
playerDataFor wid gs = do
  pid <- gs ^. antPlayer . at wid
  playerById pid gs

-- | Runs the ant if it has an associated player.
runAntIn :: GameState -> Ant -> MaybeT (State StdGen) AntOutput
runAntIn game ant = do
  playerData  <- liftMaybe $ playerDataFor (ant^.worldId) game
  let antInput = mkAntInput $ antView (ant^.datum) $ center3x3 $
                   game ^. world . worldGrid . centeredAt (positionOf ant)
  (_, ao)     <- lift $ runAntComputation antInput (playerData ^. antFunction)
  return ao

-- | Gets the outputs of all ants.
runAnts :: GameState -> State StdGen [WithPosition AntOutput]
runAnts game = do
  let ants = game ^... world . traverseAnts

  antOutputs <- forM ants $ \ant -> do
    maybeAntOutput <- runMaybeT (runAntIn game ant)
    case maybeAntOutput of
      Just ao -> return [WithPosition (positionOf ant) ao]
      Nothing -> return []

  return $ join antOutputs

-- | Just like (^..) except works with more general traversals.
infixl 8 ^...
s ^... f = runEndo [] $ getConst $ flip f s $ \a -> Const (Endo ([a]++))
