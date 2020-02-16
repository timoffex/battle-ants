{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- | Defines a type for proving that an element is in a grid at a position.
module Data.Grid.GridRef
    (
    -- * Reference a single element
      GridRef
    , refPos
    , refVal
    , gridRef

    -- * 'GridRef' in 'gridPosition'
    , gridPositionRef

    -- * Reference multiple elements
    , GridRefs
    , refs
    , someGridRefs
    , allGridRefs
    , splitRefs
    ) where

import Control.Lens (Traversal, (&), (<&>), (.~), (^?))
import Control.Utils (liftMaybe)
import Data.Maybe (isJust)
import Data.Grid.Grid
import Data.Grid.GridPosition

data GridRef g a = GridRef { refGrid :: g
                           -- ^ The referenced grid.
                           , refPos :: GridPosition
                           -- ^ The referenced position.
                           , refVal :: a
                           -- ^ The referenced value.
                           }
                   deriving (Functor)

data GridRefs g a = GridRefs { refsGrid :: g
                             -- ^ The referenced grid.
                             , refs :: [(GridPosition, a)]
                             -- ^ The referenced positions and values.
                             -- See 'splitRefs' also.
                             }
                    deriving (Functor)

-- | Returns a 'GridRef' for the position if it is within the grid.
gridRef :: ( RigidGrid g )
        => g a
        -> GridPosition
        -> Maybe (GridRef (g a) a)
gridRef g p = GridRef g p <$> (g ^? gridPosition p)

-- | Returns a 'GridRefs' for the valid given positions.
someGridRefs :: ( RigidGrid g )
             => g a
             -> [GridPosition]
             -> GridRefs (g a) a
someGridRefs g ps = GridRefs g $ do
  p <- ps
  v <- liftMaybe $ g ^? gridPosition p
  return (p, v)

-- | Returns a 'GridRefs' for all of the positions if they're all valid.
allGridRefs :: ( RigidGrid g )
            => g a
            -> [GridPosition]
            -> Maybe (GridRefs (g a) a)
allGridRefs g ps =
  let vals = fmap (\p -> g ^? gridPosition p) ps
  in GridRefs g <$> zip ps <$> sequence vals

-- | Gets individual 'GridRef's from a 'GridRefs'.
splitRefs :: GridRefs g a
          -> [GridRef g a]
splitRefs (GridRefs g vs) = vs <&> \(p, a) -> GridRef g p a

-- | Like 'gridPosition' but gets a 'GridRef g a' instead of an 'a'.
gridPositionRef :: ( RigidGrid g )
                => GridPosition
                -> Traversal (g a) (g a) (GridRef (g a) a) a
gridPositionRef p f g = case gridRef g p of
  Nothing  -> pure g
  Just ref -> gridPosition p (\_ -> f ref) g
