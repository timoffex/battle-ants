-- TODO: Partially copied for now to avoid incurring a ton of dependencies.
-- https://hackage.haskell.org/package/category-extras-0.52.0/docs/src/Control-Functor-Zip.html

module Data.Functor.Zip
  ( Zip(..)
  ) where

import Control.Monad.Identity
import Data.Functor

{- | Minimum definition:

1. fzipWith

2. fzip

-}
class Functor f => Zip f where
  fzip :: f a -> f b -> f (a, b)
  fzip = fzipWith (,)
  fzipWith :: (a -> b -> c) -> f a -> f b -> f c
  fzipWith f as bs = fmap (uncurry f) (fzip as bs)

instance Zip Identity where
  fzipWith f (Identity a) (Identity b) = Identity (f a b)

instance Zip [] where
  fzip = zip
  fzipWith = zipWith

instance Zip Maybe where
  fzipWith f (Just a) (Just b) = Just (f a b)
  fzipWith _ _ _ = Nothing

instance Monoid a => Zip ((,)a) where
  fzipWith f (a, c) (b, d) = (mappend a b, f c d)

instance Monoid a => Zip (Either a) where
  fzipWith _ (Left a) (Left b) = Left (mappend a b)
  fzipWith _ (Right _) (Left b) = Left b
  fzipWith _ (Left a) (Right _) = Left a
  fzipWith f (Right a) (Right b) = Right (f a b)
