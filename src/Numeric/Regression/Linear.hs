module Numeric.Regression.Linear
  (Model, compute, regress) where

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

-- | Compute the predicted value for
--   the given model on the given observation
compute :: (Applicative v, Foldable v, Num a)
        => Model v a -- ^ theta vector, the model's parameters
        -> v a       -- ^ @x@ vector, with the observed numbers
        -> a         -- ^ predicted @y@ for this observation
compute theta x = theta `dot` x
{-# INLINE compute #-}

-- | Cost function for a linear regression on a single observation
cost :: (Applicative v, Foldable v, Floating a)
     => Model v a -- ^ theta vector, the model's parameters
     -> v a       -- ^ @x@ vector
     -> a         -- ^ expected @y@ for the observation
     -> a         -- ^ cost
cost theta x y = 0.5 * (y - compute theta x) ^ (2 :: Int)
{-# INLINE cost #-}

-- | Cost function for a linear regression on a set of observations
totalCost :: (Applicative v, Foldable v, Applicative f, Foldable f, Floating a)
          => Model v a      -- ^ theta vector, the model's parameters
          -> f a            -- ^ expected @y@ value for each observation
          -> f (v a)        -- ^ input data for each observation
          -> a              -- ^ total cost over all observations
totalCost theta ys xs =
  let Acc n (Sum s) = foldMap acc $ liftA2 (cost theta) xs ys
  in s / fromIntegral n
{-# INLINE totalCost #-}

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
-- -- the theta we're approximating
-- realtheta :: Model V.Vector Double
-- realtheta = V.fromList [1.0, 2.0, 3.0]
--
-- -- let's start there and make 'regress'
-- -- get values that better fit the input data
-- theta0 :: Model V.Vector Double
-- theta0 = V.fromList [0.2, 3.0, 2.23]
--
-- -- input data. (output value, vector of values for each input)
-- ys_ex :: V.Vector Double
-- xs_ex :: V.Vector (V.Vector Double)
-- (ys_ex, xs_ex) = V.unzip . V.fromList $
--   [ (3, V.fromList [0, 0, 1])
--   , (1, V.fromList [1, 0, 0])
--   , (2, V.fromList [0, 1, 0])
--   , (6, V.fromList [1, 1, 1])
--   ]
--
-- -- stream of increasingly accurate parameters
-- thetaApproxs :: [Model V.Vector Double]
-- thetaApproxs = learnAll ys_ex xs_ex theta0
-- @
regress :: (Traversable v, Applicative v, Foldable v, Applicative f, Foldable f, Ord a, Floating a)
        => f a         -- ^ expected @y@ value for each observation
        -> f (v a)     -- ^ input data for each observation
        -> Model v a   -- ^ initial parameters for the model, from which we'll improve
        -> [Model v a] -- ^ a stream of increasingly accurate values
                       --   for the model's parameter to better fit the observations.
regress ys xs t0 =
  gradientDescent (\theta -> totalCost theta (fmap auto ys) (fmap (fmap auto) xs))
                  t0
{-# INLINE regress #-}
