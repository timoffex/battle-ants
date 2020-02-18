{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Grid3x3
    ( Grid3x3
    , Grid3x3Pos (..)

    -- * Constructing
    , grid3x3
    , const3x3
    , grid3x3FromList
    , center3x3

    -- * Accessing and modifying
    , ul, uc, ur
    , cl, cc, cr
    , ll, lc, lr
    , enumerate3x3
    , getGrid3x3
    , withPositions3x3

    -- * Grid3x3Pos utilities
    , grid3x3Order
    , toGridPosition
    , toXY
    , toRowColumn
    , addRowColumn
    , lens3x3At
    , argmax3x3
    ) where

import Control.Lens
import Data.Functor.Zip
import Data.Maybe (fromMaybe)
import Data.Foldable (maximumBy, toList)

import Data.GridDirection

import qualified Data.Grid as G

-- | A 3x3 grid.
data Grid3x3 a =
  Grid3x3 {
    _ul :: a, -- ^ Upper-left corner (-1, 1)
    _uc :: a, -- ^ Upper-center edge (0, 1)
    _ur :: a, -- ^ Upper-right corner (1, 1)

    _cl :: a, -- ^ Center-left edge (-1, 0)
    _cc :: a, -- ^ Center point (0, 0)
    _cr :: a, -- ^ Center-right edge (1, 0)

    _ll :: a, -- ^ Lower-left corner (-1, -1)
    _lc :: a, -- ^ Lower-center edge (0, -1)
    _lr :: a  -- ^ Lower-right corner (1, -1)
  } deriving Functor
makeLenses ''Grid3x3

-- | This is the type of ul, uc, ur, etc.
-- type GridOffset = forall a. Lens' (Grid3x3 a) a
-- TODO: Define the actual GridOffset again, and add some helper functions.

-- | Creates a 3x3 grid from 9 elements, specified in rows starting from the
-- upper left corner.
grid3x3 :: a -> a -> a
        -> a -> a -> a
        -> a -> a -> a
        -> Grid3x3 a
grid3x3 = Grid3x3

-- | Creates a 3x3 grid where all the elements are the same.
const3x3 :: a -> Grid3x3 a
const3x3 x = grid3x3 x x x x x x x x x

-- | Returns the center 3x3 elements of the given grid.
center3x3 :: (G.RigidGrid g) => g a -> Grid3x3 (Maybe a)
center3x3 grid = grid3x3FromList $
  map (\p -> grid ^? G.gridPosition (toGridPosition p)) grid3x3Order

-- | Creates a 3x3 grid from the first 9 elements of a list.
grid3x3FromList :: [a] -> Grid3x3 a
grid3x3FromList (a:b:c:d:e:f:g:h:i:rest) = grid3x3 a b c d e f g h i

-- | Enumerates the elements in order UL, UC, UR, CL, CC, CR, LL, LC, LR.
enumerate3x3 :: Grid3x3 a -> [a]
enumerate3x3 g = grid3x3Order <&> \p -> g ^. lens3x3At p

-- | Pairs every element in the grid with its position.
withPositions3x3 :: Grid3x3 a -> Grid3x3 (Grid3x3Pos, a)
withPositions3x3 grid = grid3x3FromList $ zip (grid3x3Order) (toList grid)

-- | A position on a 3x3 grid.
--
-- These correspond directly to the 'Grid3x3' accessors ('ul', 'uc', etc).
data Grid3x3Pos = UL | UC | UR
                | CL | CC | CR
                | LL | LC | LR
                deriving (Show, Eq, Ord) -- TODO Test that Ord matches grid3x3Order

class Grid3x3Position a where
  toGrid3x3Pos :: a -> Grid3x3Pos

instance {-# OVERLAPPING #-} Grid3x3Position Grid3x3Pos where
  toGrid3x3Pos = id

instance GridDirection a => Grid3x3Position a where
  toGrid3x3Pos dir = case getDirection dir of
    NW -> UL
    NN -> UC
    NE -> UR
    WW -> CL
    EE -> CR
    SW -> LL
    SS -> LC
    SE -> LR


-- | The standard order in which to enumerate a 'Grid3x3'.
grid3x3Order :: [Grid3x3Pos]
grid3x3Order = [UL, UC, UR, CL, CC, CR, LL, LC, LR]

-- | Get an element in a 3x3 grid by indexing with 'Grid3x3Pos'.
getGrid3x3 :: Grid3x3Position p => p -> Grid3x3 a -> a
getGrid3x3 p = view (lens3x3At p)

-- | Converts a 'Grid3x3Pos' to the corresponding (x, y) tuple.
toXY :: Grid3x3Pos -> (Int, Int)
toXY p = case p of
  UL -> (-1,  1)
  UC -> ( 0,  1)
  UR -> ( 1,  1)
  CL -> (-1,  0)
  CC -> ( 0,  0)
  CR -> ( 1,  0)
  LL -> (-1, -1)
  LC -> ( 0, -1)
  LR -> ( 1, -1)

fromXY :: (Int, Int) -> Maybe Grid3x3Pos
fromXY (-1,  1) = Just UL
fromXY ( 0,  1) = Just UC
fromXY ( 1,  1) = Just UR
fromXY (-1,  0) = Just CL
fromXY ( 0,  0) = Just CC
fromXY ( 1,  0) = Just CR
fromXY (-1, -1) = Just LL
fromXY ( 0, -1) = Just LC
fromXY ( 1, -1) = Just LR
fromXY _        = Nothing

-- | Converts a 'Grid3x3Pos' to the corresponding (row, column) change.
toRowColumn :: Grid3x3Pos -> (Int, Int)
toRowColumn p = case p of
  UL -> (-1, -1)
  UC -> (-1,  0)
  UR -> (-1,  1)
  CL -> ( 0, -1)
  CC -> ( 0,  0)
  CR -> ( 0,  1)
  LL -> ( 1, -1)
  LC -> ( 1,  0)
  LR -> ( 1,  1)

addRowColumn :: Grid3x3Pos -> (Int, Int) -> (Int, Int)
addRowColumn p (r, c) = (r + rp, c + cp)
  where
    (rp, cp) = toRowColumn p

-- | Returns the lens corresponding to the 'Grid3x3Position'.
--
-- 'UL' maps to 'ul', 'UC' maps to 'uc', etc.
lens3x3At :: (Grid3x3Position p) => p -> forall a. Lens' (Grid3x3 a) a
lens3x3At p = case toGrid3x3Pos p of
  UL -> ul
  UC -> uc
  UR -> ur
  CL -> cl
  CC -> cc
  CR -> cr
  LL -> ll
  LC -> lc
  LR -> lr

-- | Returns a position in the grid with a maximal value.
argmax3x3 :: (Ord a) => Grid3x3 a -> Grid3x3Pos
argmax3x3 grid = fst $ maximumBy (\(_, a) (_, b) -> compare a b)
                                 (withPositions3x3 grid)

toGridPosition :: Grid3x3Position p => p -> G.GridPosition
toGridPosition = G.fromRowColumn . toRowColumn . toGrid3x3Pos

fromGridPosition :: G.GridPosition -> Maybe Grid3x3Pos
fromGridPosition = fromXY . G.toXY

instance Zip Grid3x3 where
  fzip g1 g2 = grid3x3FromList $ zip (enumerate3x3 g1) (enumerate3x3 g2)

instance Foldable Grid3x3 where
  foldMap f = foldMap f . enumerate3x3

instance Traversable Grid3x3 where
  traverse func g = grid3x3FromList <$> traverse func (enumerate3x3 g)

instance G.RigidGrid Grid3x3 where
  type GridShape Grid3x3 = () -- Always the same shape.

  withPositions g = (\(p, a) -> (toGridPosition p, a)) <$> withPositions3x3 g

  mkGrid _ f = grid3x3FromList <$> traverse (f . toGridPosition) grid3x3Order

  shape = const ()

  gridPosition p f = fromMaybe pure $ do
    p3x3 <- fromGridPosition p
    return $ lens3x3At p3x3 f
