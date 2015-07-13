module Numeric.Regression.Internal where

import Control.Applicative
import Data.Foldable
import Data.Monoid

data Acc a = Acc {-# UNPACK #-} !Int !a

instance Monoid a => Monoid (Acc a) where
  mempty = Acc 0 mempty

  Acc m a `mappend` Acc n b = Acc (m + n) (a <> b)

acc :: a -> Acc (Sum a)
acc = Acc 1 . Sum

dot :: (Applicative v, Foldable v, Num a)
    => v a
    -> v a
    -> a
dot x y = getSum . foldMap Sum $ liftA2 (*) x y
