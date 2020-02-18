{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Defines a base API for player ant functions.
module BattleAnts.Player.AntComputation
    ( AntComputation
    , runAntComputation
    , liftRandom
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import System.Random

import BattleAnts.Player.AntInput
import BattleAnts.Player.AntOutput

-- | A computation performed by an ant.
--
-- This implements @'MonadReader' 'AntInput'@, so you can get the 'AntInput'
-- by writing 'ask'. You can temporarily change the input by using the 'local'
-- function; this can be useful to run an ant computation with a hypothetical
-- input.
--
-- This also implements @'MonadState' 'AntOutput'@, meaning you can use
-- 'get' to get the current output and 'put' to change it.
newtype AntComputation a = AntComputation
  { unAntComputation :: ReaderT AntInput
                                  (StateT AntOutput
                                    (State StdGen)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AntInput
             , MonadState AntOutput )

-- | Runs an ant computation on an input.
runAntComputation :: AntInput -> AntComputation a -> State StdGen (a, AntOutput)
runAntComputation input =
  flip runStateT mkAntOutput
    . flip runReaderT input
      . unAntComputation

-- | Performs a random computation during an 'AntComputation'.
liftRandom :: State StdGen a -> AntComputation a
liftRandom = AntComputation . lift . lift
