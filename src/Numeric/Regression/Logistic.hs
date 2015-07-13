module Numeric.Regression.Logistic
  (Model, regress) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Numeric.AD
import Numeric.Regression.Internal

-- | A model using the given @f@ to store parameters of type @a@.
--   Can be thought of as some kind of vector throughough this
--   package.
type Model f a = f a

logit :: Floating a => a -> a
logit x = 1 / (1 + exp (negate x))
{-# INLINE logit #-}

logLikelihood :: (Applicative v, Foldable v, Floating a)
              => Model v a -- theta vector
              -> a         -- y
              -> v a       -- x vector (observation)
              -> a
logLikelihood theta y x =
  y * log (logit z) + (1 - y) * log (1 - logit z)

  where z = theta `dot` x
{-# INLINE logLikelihood #-}

totalLogLikelihood :: (Applicative v, Foldable v, Applicative f, Foldable f, Floating a)
                   => a -- delta
                   -> Model v a
                   -> f a
                   -> f (v a)
                   -> a
totalLogLikelihood delta theta ys xs =
  (a - delta * b) / fromIntegral n

  where Acc n (Sum a) = foldMap acc $ liftA2 (logLikelihood theta) ys xs
        b = (/2) . getSum $ foldMap (\x -> Sum (x^(2::Int))) theta
{-# INLINE totalLogLikelihood #-}

-- | Given some observed \"predictions\" @ys@, the corresponding
--   input values @xs@ and initial values for the model's parameters @theta0@,
--
-- > regress ys xs theta0
--
-- returns a stream of values for the parameters that'll fit the data better
-- and better.
--
-- Example:
--
-- @
-- ys_ex :: [Double]
-- xs_ex :: [[Double]]
-- (ys_ex, xs_ex) = unzip $
--   [ (1, [1, 1])
--   , (0, [-1, -2])
--   , (1, [2, 5])
--   , (0, [-1, 1])
--   , (1, [2, -1])
--   , (1, [1, -10])
--   , (0, [-0.1, 30])
--   ]
--
-- t0 :: [Double]
-- t0 = [1, 0.1]
--
-- approxs' :: [Model [] Double]
-- approxs' = learn 0.1 ys_ex xs_ex t0
-- @
regress :: (Traversable v, Applicative v, Foldable f, Applicative f, Ord a, Floating a)
        => a           -- ^ learning rate
        -> f a         -- ^ expect prediction for each observation
        -> f (v a)     -- ^ input data for each observation
        -> Model v a   -- ^ initial values for the model's parameters
        -> [Model v a] -- ^ stream of increasingly accurate values for
                       --   the model's parameters
regress delta ys xs =
  gradientAscent $ \theta ->
    totalLogLikelihood (auto delta) theta (fmap auto ys) (fmap (fmap auto) xs)
{-# INLINE regress #-}
