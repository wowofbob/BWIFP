{-# LANGUAGE DeriveFunctor #-}
module BWIFP.Chp1 where

import Prelude hiding (max, pi, pred, succ)


{- 1.1.1 Sessions and scripts. -}

-- 1.1.1

square :: Num a => a -> a
square x = x * x

quad :: Num a => a -> a
quad x = square x * square x


-- 1.1.2

max :: Ord a => a -> a -> a
max x y
  | x > y     = x
  | otherwise = y


-- 1.1.3

pi :: Fractional a => a
pi = 22 / 7

circleArea :: Fractional a => a -> a
circleArea r = pi * square r


{- 1.2.1 Reduction. -}

-- 1.2.3

newtype Fix f = Fix { unFix :: f (Fix f) }

data Number a = Zero | Succ a | Pred a
  deriving Functor

type Expr = Fix Number

zero :: Expr
zero = Fix Zero

succ :: Expr -> Expr
succ = Fix . Succ

pred :: Expr -> Expr
pred = Fix . Pred

expr :: Expr
expr = succ $ pred $ succ $ pred $ pred zero
