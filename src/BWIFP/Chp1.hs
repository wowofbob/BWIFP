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
-- https://github.com/sellout/recursion-scheme-talk/blob/master/nanopass-compiler-talk.org
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

evalExprAlg :: Num a => ExprEvalAlgebra a 
evalExprAlg Zero     = 0
evalExprAlg (Succ n) = n + 1
evalExprAlg (Pred n) = n - 1


-- Definition of initial algebra.
type InitAlgebra f = Algebra f (Fix f)

initAlg :: InitAlgebra f
initAlg = Fix


-- Initial algebra for expressions.
type ExprInitAlgebra = InitAlgebra ExprF


-- Catamorphism.
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix


-- Evaluator for expressions.
evalExpr :: Num a => Expr -> a
evalExpr = cata evalExprAlg


-- Algebra which simplifies expressions.
type ExprSimplifyAlg = Algebra ExprF Expr

simplifyExprAlg :: ExprSimplifyAlg
simplifyExprAlg (Succ (Fix (Pred e))) = e
simplifyExprAlg (Pred (Fix (Succ e))) = e
simplifyExprAlg e                     = Fix e


-- Simplifier for expressions.
simplifyExpr :: Expr -> Expr
simplifyExpr = cata simplifyExprAlg


-- Simplified expression.
expr' :: Expr
expr' = simplifyExpr expr


-- Algebra which converts expressions to strings.
type ExprRenderAlg = Algebra ExprF String

renderExprAlg :: ExprRenderAlg
renderExprAlg Zero = "zero"
renderExprAlg (Succ str) = "Succ (" ++ str ++ ")"
renderExprAlg (Pred str) = "Pred (" ++ str ++ ")"


-- Renderer for expressions.
renderExpr :: Expr -> String
renderExpr = cata renderExprAlg


-- Algebra which compute a length of expression.
type ExprLengthAlg a = Algebra ExprF a

exprLengthAlg :: Num a => ExprLengthAlg a
exprLengthAlg Zero     = 1
exprLengthAlg (Succ n) = n + 1
exprLengthAlg (Pred n) = n + 1


-- Lenght of expression.
exprLength :: Num a => Expr -> a
exprLength = cata exprLengthAlg


-- Counts simplification apllications (two-pass).
countSimplifications :: Integral a => Expr -> a
countSimplifications e =
  let e'    = simplifyExpr e
      eLen  = exprLength e
      eLen' = exprLength e'
      in (eLen - eLen') `div` 2 -- Each simplication removes two operators.

-- Counts simplification applications (one-pass).
simplifyExprAlg' :: Algebra ExprF (Int, Expr)
simplifyExprAlg' (Succ (n, (Fix (Pred e)))) = (n + 1, e)
simplifyExprAlg' (Pred (n, (Fix (Succ e)))) = (n + 1, e)
simplifyExprAlg' (Succ (n, e))              = (n, succ e)
simplifyExprAlg' (Pred (n, e))              = (n, pred e)
simplifyExprAlg' Zero                       = (0, zero)

countSimplifications' :: Expr -> Int
countSimplifications' = fst . cata simplifyExprAlg'


-- 1.2.4
data Post = I | O deriving Show

type PostExpr = [Post]

simplifyPostExpr :: PostExpr -> PostExpr
simplifyPostExpr (I : _ : _ : x) = x ++ [I, I, O, I]
simplifyPostExpr (O : _ : _ : x) = x ++ [O, O]
simplifyPostExpr x               = x

