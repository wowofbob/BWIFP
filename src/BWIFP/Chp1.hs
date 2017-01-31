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
-- https://www.schoolofhaskell.com/user/bartosz/understanding-algebras

-- Functor's fixed point.
newtype Fix f = Fix { unFix :: f (Fix f) }

-- Defenition of expression.
data ExprF a = Zero | Succ a | Pred a
  deriving Functor

type Expr = Fix ExprF

zero :: Expr
zero = Fix Zero

succ :: Expr -> Expr
succ = Fix . Succ

pred :: Expr -> Expr
pred = Fix . Pred


-- Example expression which should be simplified.
expr :: Expr
expr = succ $ pred $ succ $ pred $ pred zero


-- Definition of algebra.
type Algebra f a = f a -> a


-- Algebra which evaluates expressions.
type ExprEvalAlgebra a = Algebra ExprF a

evalNode :: Num a => ExprEvalAlgebra a 
evalNode Zero     = 0
evalNode (Succ n) = n + 1
evalNode (Pred n) = n - 1


-- Definition of initial algebra.
type InitAlgebra f = Algebra f (Fix f)

initAlg :: InitAlgebra f
initAlg = Fix


-- Initial algebra for expressions.
type ExprInitAlgebra = InitAlgebra ExprF
