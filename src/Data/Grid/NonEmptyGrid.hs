{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Grid.NonEmptyGrid
    ( NonEmptyGrid
    , grid
    , center
    , nonEmptyGrid
    , maybeNonEmpty
    , duplicateGrid
    , extendGrid
    ) where

import NumHask.Prelude hiding (Grid, grid, to)

import Control.Lens
import Control.Comonad
import Control.Comonad.Cofree
import Data.Grid.Grid
import Data.Maybe (fromJust)

-- | A grid that has an element at (0, 0).
data NonEmptyGrid g a = NonEmptyGrid a (g a)
                        deriving (Functor)

gridLens :: Lens' (NonEmptyGrid g a) (g a)
gridLens = lens grid setGrid
  where
    setGrid nonEmpty g = nonEmptyGrid (center nonEmpty) g

-- | Gets the underlying grid from a 'NonEmptyGrid'.
grid :: NonEmptyGrid g a -> g a
grid (NonEmptyGrid _ g) = g

-- | Gets the center element of a 'NonEmptyGrid'.
center :: NonEmptyGrid g a -> a
center (NonEmptyGrid c _) = c

-- | Creates a 'NonEmptyGrid' given a grid and its center element.
--
-- This assumes that the given element is at the center of the grid. For an
-- alternative that does not make this assumption, see 'maybeNonEmpty'.
nonEmptyGrid :: a -> g a -> NonEmptyGrid g a
nonEmptyGrid = NonEmptyGrid

-- | Just a 'NonEmptyGrid' given a non-empty 'Grid'; otherwise Nothing.
maybeNonEmpty :: RigidGrid g => g a -> Maybe (NonEmptyGrid g a)
maybeNonEmpty g = do
  center <- g ^? gridPosition zero
  return $ nonEmptyGrid center g

duplicateGrid :: Grid g => g a -> g (NonEmptyGrid g a)
duplicateGrid g = flip mapWithPositions g $
                     \p a -> nonEmptyGrid a (g ^. centeredAt p)

extendGrid :: Grid g => (NonEmptyGrid g a -> b) -> g a -> g b
extendGrid f g = f <$> duplicateGrid g

instance Foldable g => Foldable (NonEmptyGrid g) where
  foldMap f = foldMap f . grid

instance RigidGrid g => Traversable (NonEmptyGrid g) where
  traverse f nonEmpty = let result = traverse f (grid nonEmpty)
                        in result <&> fromJust . maybeNonEmpty

instance RigidGrid g => RigidGrid (NonEmptyGrid g) where
  type GridShape (NonEmptyGrid g) = GridShape g

  gridPosition p = gridLens . gridPosition p

  mapWithPositions f g = nonEmptyGrid (f zero $ center g)
                                      (mapWithPositions f $ grid g)

  mkGrid s f = do
    g <- mkGrid s f
    maybeNonEmpty g

  shape = shape . grid

instance Grid g => Comonad (NonEmptyGrid g) where
  extract = center
  duplicate nonEmpty = let g = grid nonEmpty
                       in nonEmptyGrid nonEmpty $
                            flip mapWithPositions g $
                              \p a -> nonEmptyGrid a (g ^. centeredAt p)
