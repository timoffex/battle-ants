{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the 'WorldId' type for representing IDs.
module BattleAnts.World.WorldId
    ( WorldId
    , newWorldId

    , WithId (WithId)
    , HasWorldId (worldId)
    , HasDatum (datum)
    ) where

import Control.Lens
import Data.Unique

import BattleAnts.Fields.HasDatum

-- | An ID in the world.
newtype WorldId = WorldId Unique
                  deriving (Eq, Ord)

-- | Generates a new 'WorldId' that has not been generated before.
newWorldId :: IO WorldId
newWorldId = WorldId <$> newUnique

-- | Attaches a 'WorldId' to a piece of data.
data WithId d = WithId
  { _withIdWorldId  :: WorldId
  , _withIdDatum    :: d
  } deriving (Functor)
makeFields ''WithId
