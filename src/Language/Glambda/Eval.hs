{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables,
             DataKinds, TypeFamilies, PolyKinds,
             GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Eval
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Glambda expression evaluators for checked expressions.
--
----------------------------------------------------------------------------

module Language.Glambda.Eval ( eval, step ) where

import Language.Glambda.Exp
import Language.Glambda.Token
import Language.Glambda.Shift

-- | Given a lambda and an expression, beta-reduce.
apply :: Val (arg -> res) -> Exp '[] arg -> Exp '[] res
apply body arg = subst arg body

-- | Apply an arithmetic operator to two values.
arith :: Val Int -> ArithOp ty -> Val Int -> Exp '[] ty
arith n1 Plus     n2 = IntE (n1 + n2)
arith n1 Minus    n2 = IntE (n1 - n2)
arith n1 Times    n2 = IntE (n1 * n2)
arith n1 Divide   n2 = IntE (n1 `div` n2)
arith n1 Mod      n2 = IntE (n1 `mod` n2)
arith n1 Less     n2 = BoolE (n1 < n2)
arith n1 LessE    n2 = BoolE (n1 <= n2)
arith n1 Greater  n2 = BoolE (n1 > n2)
arith n1 GreaterE n2 = BoolE (n1 >= n2)
arith n1 Equals   n2 = BoolE (n1 == n2)

-- | Conditionally choose between two expressions
cond :: Val Bool -> Exp '[] t -> Exp '[] t -> Exp '[] t
cond True  e _ = e
cond False _ e = e

-- | Unroll a `fix` one level
unfix :: Val (ty -> ty) -> Exp '[] ty
unfix body = subst (Fix (Lam body)) body

-- | A well-typed variable in an empty context is impossible.
impossibleVar :: Elem '[] x -> a
impossibleVar _ = error "GHC's typechecker failed"
  -- GHC 7.8+ supports EmptyCase for this, but the warnings for that
  -- construct don't work yet.

-- | Evaluate an expression, using big-step semantics.
eval :: Exp '[] t -> Val t
eval (Var v)          = impossibleVar v
eval (Lam body)       = body
eval (App e1 e2)      = eval (apply (eval e1) e2)
eval (Arith e1 op e2) = eval (arith (eval e1) op (eval e2))
eval (Cond e1 e2 e3)  = eval (cond (eval e1) e2 e3)
eval (Fix e)          = eval (unfix (eval e))
eval (IntE n)         = n
eval (BoolE b)        = b

-- | Step an expression, either to another expression or to a value.
step :: Exp '[] t -> Either (Exp '[] t) (Val t)
step (Var v)          = impossibleVar v
step (Lam body)       = Right body
step (App e1 e2)      = case step e1 of
                          Left e1' -> Left (App e1' e2)
                          Right body -> Left (subst e2 body)
step (Arith e1 op e2) = case step e1 of
                          Left e1' -> Left (Arith e1' op e2)
                          Right v1 -> case step e2 of
                            Left e2' -> Left (Arith (val v1) op e2')
                            Right v2 -> Left (arith v1 op v2)
step (Cond e1 e2 e3)  = case step e1 of
                          Left e1' -> Left (Cond e1' e2 e3)
                          Right v1 -> Left (cond v1 e2 e3)
step (Fix e)          = case step e of
                          Left e' -> Left (Fix e')
                          Right v -> Left (unfix v)
step (IntE n)         = Right n
step (BoolE b)        = Right b
