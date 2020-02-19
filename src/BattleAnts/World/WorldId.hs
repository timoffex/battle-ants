{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the 'WorldId' type for representing IDs.
module BattleAnts.World.WorldId
    ( WorldId (WorldId)
    , fromId

    , WithId (WithId)
    , HasWorldId (worldId)
    , HasDatum (datum)
    ) where

import Control.Lens

import BattleAnts.Fields.HasDatum

-- | An ID in the world.
newtype WorldId = WorldId { fromId :: Int }
                  deriving (Eq, Ord)

-- | Attaches a 'WorldId' to a piece of data.
data WithId d = WithId
  { _withIdWorldId  :: WorldId
  , _withIdDatum    :: d
  } deriving (Functor)
makeFields ''WithId
