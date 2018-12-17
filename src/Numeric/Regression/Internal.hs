module Numeric.Regression.Internal where

import Control.Applicative
import Data.Monoid

data Acc a = Acc {-# UNPACK #-} !Int !a

instance Semigroup a => Semigroup (Acc a) where
  Acc m a <> Acc n b = Acc (m + n) (a <> b)

instance Monoid a => Monoid (Acc a) where
  mempty = Acc 0 mempty

acc :: a -> Acc (Sum a)
acc = Acc 1 . Sum

dot :: (Applicative v, Foldable v, Num a)
    => v a
    -> v a
    -> a
dot x y = getSum . foldMap Sum $ liftA2 (*) x y
