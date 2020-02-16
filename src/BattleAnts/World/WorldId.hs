-- | Defines the 'WorldId' type for representing IDs.
module BattleAnts.World.WorldId
    ( WorldId (WorldId)
    , fromId
    ) where

-- | An ID in the world.
newtype WorldId = WorldId { fromId :: Int }
