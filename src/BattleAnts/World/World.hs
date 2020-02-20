{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines the 'World' type.
module BattleAnts.World.World
    ( WorldCellData
    , mkWorldCellData
    , HasCell (cell)
    , HasEntity (entity)

    , World
    , mkWorld
    , HasWorldGrid (worldGrid)

    , allWorldIdsOf
    , traverseIdsEntities
    , traverseEntities
    , traverseIdsAnts
    , traverseAnts
    ) where

import Control.Lens
import Data.Functor.Compose
import Data.Grid
import qualified Data.Set as S
import Data.SparseGrid

import BattleAnts.World.Ant
import BattleAnts.World.Cell
import BattleAnts.World.Entity
import BattleAnts.World.WorldId
import BattleAnts.World.WorldObject

-- | All data associated with a cell in the world.
data WorldCellData = WorldCellData
  { _worldCellDataCell   :: CellData
  , _worldCellDataEntity :: Maybe (WithId EntityData)
  }
makeFields ''WorldCellData

mkWorldCellData :: CellData -> WorldCellData
mkWorldCellData cd = WorldCellData cd Nothing

data World = World
  { _worldWorldGrid :: SparseGrid WorldCellData
  }
makeFields ''World

mkWorld :: SparseGrid WorldCellData -> World
mkWorld = World

-- | Gets all 'WorldId's present in the given 'World'.
allWorldIdsOf :: World -> S.Set WorldId
allWorldIdsOf world = S.fromList $
  world ^.. worldGrid
          . traverse
          . entity . _Just . worldId

-- | Isomorphism between 'WorldObject d' and @('GridPosition', 'WithId' 'd')@.
worldObject :: Iso' (GridPosition, WithId d) (WorldObject d)
worldObject = iso toWorldObject fromWorldObject
  where
    toWorldObject    (p, WithId i d)      = (WorldObject i p d)
    fromWorldObject  (WorldObject i p d)  = (p, WithId i d)

-- | Lifts a lens-like to view a 'WorldObject'.
--
-- Allows getting a 'WorldObject' and setting a value wrapped in 'g'.
-- Practically, 'g' is either 'WorldObject', 'WithId', or 'Identity'.
viewObject :: LensLike (Compose f g) s t a b
           -> LensLike f (WorldObject s) (g t)
                         (WorldObject a) (g b)
viewObject l f (WorldObject i p a) =
  getCompose $ flip l a $ \b -> Compose $ f (WorldObject i p b)

-- | Disallows setting positions in a lens.
withoutPositions :: Lens (WorldObject s) (WorldObject t)
                         (WorldObject s) (WithId t)
withoutPositions = lens id setValue
  where
    setValue (WorldObject _ p _) (WithId i t) = WorldObject i p t

-- | Disallows setting IDs in a lens (when positions are already not allowed).
withoutIds :: Lens (WorldObject s) (WithId t) (WorldObject s) t
withoutIds = lens id setValue
  where
    setValue (WorldObject i _ _) t = WithId i t

-- | Allows viewing the 'Entity's in the world and modifying them and their IDs.
--
-- While this allows getting 'Entity', it does not allow changing their
-- positions.
traverseIdsEntities :: Traversal World World Entity (WithId EntityData)
traverseIdsEntities = worldGrid
                    . (itraversed <. entity . _Just) . withIndex
                    . worldObject
                    . withoutPositions

-- | Allows viewing the 'Entity's in the world and modifying them.
--
-- Does not allow modifying their IDs.
traverseEntities :: Traversal World World Entity EntityData
traverseEntities = traverseIdsEntities . withoutIds

-- | Allows viewing the 'Ant's in the world and modifying them and their IDs.
--
-- Allows getting the full 'Ant' object but only setting the ID and data.
--
-- For instance,
--
-- @
--   -- Gets the list of ants in the world.
--   world ^.. traverseAnts :: [Ant]
--
--   -- Replaces all ants by 'myAnt', without changing their IDs.
--   traverseAnts %~ fmap (const myAnt) :: World -> World
-- @
traverseIdsAnts :: Traversal World World Ant (WithId AntData)
traverseIdsAnts = traverseIdsEntities . viewObject _EntityAnt

-- | Allows viewing the 'Ant's in the world and modifying them.
--
-- Does not allow modifying their IDs.
traverseAnts :: Traversal World World Ant AntData
traverseAnts = traverseIdsAnts . withoutIds
