{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines the 'datum' field.
module BattleAnts.Fields.HasDatum
    ( HasDatum (datum)
    ) where

import Control.Lens

class HasDatum a b | a -> b where
  datum :: Lens' a b
