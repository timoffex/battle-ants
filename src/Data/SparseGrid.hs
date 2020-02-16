{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SparseGrid
    ( SparseGrid
    , emptySparseGrid
    , adjustOverlap
    , fromEntries

    -- * Conversions
    , getMap
    , ConvertibleToSparseGrid (sparseGrid)
    ) where

import NumHask.Prelude hiding (Grid)
import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable ( Foldable (..) )
import Data.Key ( Key
                , FoldableWithKey (..)
                , TraversableWithKey (..)
                , Keyed (..)
                , Lookup (..)
                , Adjustable (..) )
import Data.Maybe ( maybe, fromJust )

import Data.Grid ( Grid (..)
                 , RigidGrid (..)
                 , GridPosition
                 , toXY
                 , fromXY )

-- TODO: Consider using an IntMap, or at least a strict map.
-- | A finite grid internally represented by a map.
--
-- Updating an element is @O(log n)@ where @n@ is the number of elements in the
-- grid.
data SparseGrid a = SparseGrid (M.Map GridPosition a)
                    deriving (Functor, Show, Eq)

-- | An empty grid.
emptySparseGrid :: SparseGrid a
emptySparseGrid = SparseGrid M.empty

-- | The grid's underlying map.
getMap :: SparseGrid a -> M.Map GridPosition a
getMap (SparseGrid m) = m

-- | Creates a 'SparseGrid' from a list of key-value pairs.
--
-- If there are multiple elements with the same key, the last value is retained.
fromEntries :: [(GridPosition, a)] -> SparseGrid a
fromEntries = SparseGrid . M.fromList

-- | Applies the function wherever the two grids overlap.
--
-- The resulting grid will have the same shape as the /right/ grid, i.e. if the
-- right grid has an element at (x, y), then so does the right grid.
adjustOverlap :: (a -> b -> b)
              -> SparseGrid a
              -> SparseGrid b
              -> SparseGrid b
adjustOverlap f g1 g2 = SparseGrid $ M.mapWithKey adjust $ getMap g2
  where
    adjust p b = case M.lookup p (getMap g1) of
      Nothing -> b
      Just a  -> f a b

-- | The same grid where every element is replaced by its key.
positionsOnly :: SparseGrid a -> SparseGrid GridPosition
positionsOnly g = mapWithKey (\k _ -> k) g

-- | Shifts every element in the grid so that the given position becomes (0, 0).
shiftGrid :: SparseGrid a -> GridPosition -> SparseGrid a
shiftGrid g p = fromEntries $ toList $ flip mapWithKey g $
  \k a -> (k - p, a)


instance FunctorWithIndex GridPosition SparseGrid where
  imap = mapWithKey
instance FoldableWithIndex GridPosition SparseGrid where
  ifoldMap = foldMapWithKey
instance TraversableWithIndex GridPosition SparseGrid where
  itraverse = traverseWithKey

-- | Allows creating a 'sparseGrid' from some other data.
class ConvertibleToSparseGrid t a | t -> a where
  -- | Creates a 'SparseGrid' from the given data.
  sparseGrid :: t -> SparseGrid a

instance ConvertibleToSparseGrid (M.Map GridPosition a) a where
  -- | Creates a 'SparseGrid' from a 'M.Map'.
  sparseGrid = SparseGrid

instance Foldable SparseGrid where
  foldMap f = foldMap f . getMap

instance FoldableWithKey SparseGrid where
  toKeyedList = M.assocs . getMap
  foldMapWithKey f = M.foldMapWithKey f . getMap
  foldrWithKey f i = M.foldrWithKey f i . getMap
  foldlWithKey f i = M.foldlWithKey f i . getMap

instance Traversable SparseGrid where
  traverse func g = SparseGrid <$> traverse func (getMap g)

instance TraversableWithKey SparseGrid where
  traverseWithKey f = fmap SparseGrid . M.traverseWithKey f . getMap


type instance Key SparseGrid = GridPosition

instance Keyed SparseGrid where
  mapWithKey f = SparseGrid . M.mapWithKey f . getMap

instance Lookup SparseGrid where
  lookup k = M.lookup k . getMap

instance Adjustable SparseGrid where
  adjust f k = SparseGrid . M.adjust f k . getMap

instance RigidGrid SparseGrid where
  type GridShape SparseGrid = S.Set GridPosition

  gridPosition p f g =
    let m = getMap g
    in case M.lookup p m of
      Nothing -> pure g
      Just a  -> fmap (\a -> SparseGrid $ M.insert p a m) $ f a

  mapWithPositions f g = SparseGrid $ M.mapWithKey f $ getMap g

  mkGrid s f = maybe Nothing (Just . SparseGrid . M.fromList)
                             (mapM toEntry $ S.toList s)
    where
      toEntry p = case f p of
        Nothing -> Nothing
        Just a  -> Just (p, a)

  shape = S.fromList . M.keys . getMap

instance Grid SparseGrid where
  centeredAt p = lens getShifted setShifted
    where
      getShifted g = shiftGrid g p
      setShifted _ g' = shiftGrid g' (negate p)
