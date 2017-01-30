{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (max, pi)

import Control.Functor.Fix

-- 1.1.1 Sessions and scripts.

square :: Num a => a -> a
square x = x * x

quad :: Num a => a -> a
quad x = square x * square x

max :: Ord a => a -> a -> a
max x y
  | x > y     = x
  | otherwise = y
  
pi :: Fractional a => a
pi = 22 / 7

circleArea :: Fractional a => a -> a
circleArea r = pi * square r


-- 1.2.1 Reduction.
{-
data Zero f a = Zero a deriving Functor
data Succ f a = Succ a deriving Functor
data Pred f a = Pred a deriving Functor
-}
data ExprF f a = ZeroF | SuccF a | PredF a

