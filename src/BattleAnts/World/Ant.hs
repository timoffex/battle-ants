-- | Defines the data of an ant.
module BattleAnts.World.Ant
    ( Ant
    , AntData
    , mkAntData
    ) where

import BattleAnts.World.WorldObject

-- | The data of an ant.
data AntData = AntData ()

mkAntData :: AntData
mkAntData = AntData ()

type Ant = WorldObject AntData
