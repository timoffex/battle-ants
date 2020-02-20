-- | Defines utilities for working with lenses.
module Control.LensUtils
    ( (^...)
    ) where

import Control.Lens
import Data.Functor.Const
import Data.Monoid.Endo

infixl 8 ^...

-- | Just like '(^..)' except works with more general getters.
--
-- You can view this as having the specific type
-- @
--   s -> Traversal s t a b -> [a]
-- @
--
-- This is necessary because '(^..)' expects a @'Getter' s a@, and a
-- @'Traversal' s t a b@, though almost correct, is not actually that type.
(^...) ::  s
       -> ((a -> Const (Endo [a]) b) -> s -> Const (Endo [c]) t)
       -> [c]
s ^... f = runEndo [] $ getConst $ flip f s $ \a -> Const (Endo ([a]++))
