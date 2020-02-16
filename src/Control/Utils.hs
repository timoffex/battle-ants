-- | Some utility functions for dealing with control structures.
module Control.Utils
    ( liftMaybe
    ) where

import Control.Monad

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe Nothing  = mzero
liftMaybe (Just a) = return a
