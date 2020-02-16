{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Grid.Grid
    ( RigidGrid (..)
    , Grid (..)
    ) where

import Control.Lens
import NumHask.Prelude hiding (Grid)

import Data.Grid.GridPosition

-- | A type whose values are grids.
--
-- "Rigid" means not necessarily supporting translation / rotation / etc.
-- Minimal complete definition: @gridPosition, (mapWithPositions|withPositions),
-- mkGrid, shape@.
class (Traversable g) => RigidGrid g where
  -- | A type that represents the shape of the grid.
  --
  -- This should be just enough information to construct a grid of the same
  -- shape.
  type GridShape g

  gridPosition :: GridPosition -> Traversal' (g a) a

  withPositions :: g a -> g (GridPosition, a)
  withPositions = mapWithPositions (,)

  mapWithPositions :: (GridPosition -> a -> b) -> g a -> g b
  mapWithPositions f = fmap (uncurry f) . withPositions

  -- | Matches functions from the first grid to values in the second grid.
  --
  -- Succeeds iff the shape is a sub-shape of both grids.
  apply :: GridShape g -> g (a -> b) -> g a -> Maybe (g b)
  apply s gf ga = mkGrid s $ \p -> do
    f <- gf ^? gridPosition p
    a <- ga ^? gridPosition p
    return $ f a

  -- | Like 'apply' but uses the shape of the functions grid.
  applyAll :: g (a -> b) -> g a -> Maybe (g b)
  applyAll gf ga = apply (shape gf) gf ga

  -- | Like 'apply' but uses the shape of the values grid.
  applyTo :: g (a -> b) -> g a -> Maybe (g b)
  applyTo gf ga = apply (shape ga) gf ga

  traverseWithPositions :: (Applicative f)
                        => (GridPosition -> a -> f b)
                        -> g a
                        -> f (g b)
  traverseWithPositions f g =
    let gp = mapWithPositions (\p a -> (p, a)) g
    in traverse (uncurry f) gp

  -- | Creates a grid of the given shape.
  --
  -- Succeeds iff the function is a 'Just' value at every position in the shape.
  mkGrid :: GridShape g -> (GridPosition -> Maybe a) -> Maybe (g a)

  shape :: g a -> GridShape g

-- | A type that is somewhat like @(Int, Int) -> Maybe a@.
--
-- The reason it is "somewhat" like that is because it does not have to include
-- every possible function @(Int, Int) -> Maybe a@. It could be a type of
-- rectangular grids, for example, or it could be a type of grids of a
-- particular size and shape.
class (RigidGrid g) => Grid g where
  centeredAt :: GridPosition -> Lens' (g a) (g a)
