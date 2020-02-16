{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines transition that moves entities.
module BattleAnts.World.Transitions.MoveEntities
    ( moveEntities
    , MoveResult
    , HasConflicts (conflicts)
    , HasInvalidDests (invalidDests)
    , HasInvalidOrigins (invalidOrigins)
    , Origin (Origin)
    , Destination (Destination)
    ) where

import Control.Lens
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Utils
import Data.Coerce
import Data.Grid
import Data.SparseGrid
import Data.Maybe
import Data.STRef
import qualified Data.Set as S
import qualified Data.Map as M

import BattleAnts.World.World

newtype Origin = Origin GridPosition
                 deriving (Eq, Ord)
newtype Destination = Destination GridPosition
                      deriving (Eq, Ord)

newtype Conflicts    = Conflicts (M.Map Destination [Origin])
newtype InvalidDests = InvalidDests (M.Map Destination [Origin])

-- | The result of 'moveEntities'.
--
-- Movements can be invalid for three reasons:
--
-- * There is no entity at the origin.
-- * The destination is occupied either by a non-moving entity or by an
--   entity that failed to move.
-- * There are multiple movements to the same destination (a conflict)
--   (which would succeed if there were only one).
data MoveResult = MoveResult
  { _moveResultConflicts      :: Conflicts
  , _moveResultInvalidDests   :: InvalidDests
  , _moveResultInvalidOrigins :: M.Map Origin Destination }
makeFields ''MoveResult

-- | Intermediate data structure for performing movement algorithm.
data DestinationStatus = Invalid
                       -- ^ Invalid position, or occupied by non-moving entity.
                       | Unoccupied
                       -- ^ Position not occupied.
                       | SingleMover Origin
                       -- ^ One entity already moved to the position.
                       | Conflict [Origin]
                       -- ^ There is a conflict.
makePrisms ''DestinationStatus

-- | Simultaneously moves entities from 'Origin's to 'Destinations'.
--
-- Any movements that could not be performed are returned in 'MoveResult'.
moveEntities :: M.Map Origin Destination -> State World MoveResult
moveEntities originDestMap = do
  world <- get

  -- Compute movements that can and cannot be done.
  let (result, goodMovements) = computeMovements originDestMap world

  forM_ goodMovements $ \(o, d) ->
    moveEntity o d

  return result

-- | Computes which movements can and cannot happen in the world.
--
-- Given a map of movements (origin and destination pairs) and a world,
-- computes conflicts, invalid destinations and origins, and valid moves.
computeMovements :: M.Map Origin Destination
                 -> World
                 -> (MoveResult, [(Origin, Destination)])
computeMovements originDestMap world = runST $ do
  {- The essence of the algorithm:

     1. Place all non-moving entities. This is represented by setting the
        destination status to 'Invalid' at destinations with a non-moving
        entity and 'Unoccupied' otherwise.

     2. One-by-one, put down moving entities at their destinations as follows:
      2a. If the destination is unoccupied, put the entity there by setting
          the destination status to 'SingleMover'.
      2b. If the destination is occupied by a non-moving entity or is not a
          valid position on the grid, revert the entity to its origin.
      2c. If the destination is occupied by a moving entity, revert both
          entities and set the destination status to 'Conflict'.
      2d. If the destination status is 'Conflict', just revert the entity to
          its origin.

     To revert an entity:
       First, revert any moving entity that was put at its origin.
       Then, set the status at the origin as 'Invalid' to represent that there
       is a non-moving entity there.
  -}

  invalidDestsRef   <- newSTRef M.empty
  invalidOriginsRef <- newSTRef M.empty
  destStatusesRef   <- newSTRef M.empty

  let getDestinationStatusAt d = do
        destStatuses <- readSTRef destStatusesRef
        case M.lookup d destStatuses of
          Just s  -> return s
          Nothing -> if canPlaceEntityAt (coerce d) world
                          || M.member (coerce d) originDestMap
                       then return Unoccupied
                       else return Invalid

      setDestinationStatusAt d s = do
        modifySTRef destStatusesRef $ M.insert d s

      markInvalid (o, d) = do
        modifySTRef invalidDestsRef $ M.insertWith (++) d [o]

      -- Places entity at its origin, reverting anything that was put there.
      revert o = do
        stat <- getDestinationStatusAt (coerce o)
        setDestinationStatusAt (coerce o) Invalid
        case stat of
          SingleMover o' -> revert o'
          Conflict os    -> mapM_ revert os
          Unoccupied     -> return ()
          Invalid        -> error "Could not place entity at its origin."

  forM_ (M.assocs originDestMap) $ \(o, d) ->
    if not $ entityExistsAt (coerce o) world
      then do
        -- No entity at the origin, so this is just an invalid movement.
        modifySTRef invalidOriginsRef $ M.insert o d
      else do
        stat <- getDestinationStatusAt d
        case stat of
          Invalid         -> do
            markInvalid (o, d)
            revert o
          Unoccupied      -> do
            setDestinationStatusAt d $ SingleMover o
          SingleMover o'  -> do
            setDestinationStatusAt d $ Conflict [o, o']
            revert o
            revert o'
          Conflict os     -> do
            setDestinationStatusAt d $ Conflict (o:os)
            revert o

  invalidDests   <- readSTRef invalidDestsRef
  destStatuses   <- readSTRef destStatusesRef
  invalidOrigins <- readSTRef invalidOriginsRef

  let conflicts = mconcat $ M.assocs destStatuses
                              ^.. traverse
                                . aside _Conflict
                                . to (uncurry M.singleton)
      goodMoves = M.assocs destStatuses ^.. traverse
                                          . aside _SingleMover
                                          . swapped
  return ( MoveResult
             (Conflicts conflicts)
             (InvalidDests invalidDests)
             invalidOrigins
         , goodMoves )

-- | Checks whether there is an entity at the given position in the world.
entityExistsAt :: GridPosition -> World -> Bool
entityExistsAt p w =
  case w ^? worldGrid . gridPosition p . entity of
    Just (Just _) -> True
    _             -> False

-- | Checks whether the position exists and is devoid of entities.
canPlaceEntityAt :: GridPosition -> World -> Bool
canPlaceEntityAt p w =
  case w ^? worldGrid . gridPosition p . entity of
    Just Nothing -> True
    _            -> False

-- | Moves the entity at the origin to the destination.
--
-- This assumes that there is an entity at the origin and no entity at the
-- destination.
moveEntity :: Origin -> Destination -> State World ()
moveEntity o d = void $ runMaybeT $ do
  world <- lift get
  e     <- liftMaybe $ world ^? worldGrid . gridPosition (coerce o) . entity
  put $ world & worldGrid . gridPosition (coerce o) . entity .~ Nothing
              & worldGrid . gridPosition (coerce d) . entity .~ e
