{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables,
             DataKinds, TypeFamilies, PolyKinds,
             GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Eval
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
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
apply = undefined

-- | Apply an arithmetic operator to two values.
arith :: Val Int -> ArithOp ty -> Val Int -> Exp '[] ty
arith (IntVal n1) Plus     (IntVal n2) = IntE (n1 + n2)
arith (IntVal n1) Minus    (IntVal n2) = IntE (n1 - n2)
arith (IntVal n1) Times    (IntVal n2) = IntE (n1 * n2)
arith (IntVal n1) Divide   (IntVal n2) = IntE (n1 `div` n2)
arith (IntVal n1) Mod      (IntVal n2) = IntE (n1 `mod` n2)
arith (IntVal n1) Less     (IntVal n2) = BoolE (n1 < n2)
arith (IntVal n1) LessE    (IntVal n2) = BoolE (n1 <= n2)
arith (IntVal n1) Greater  (IntVal n2) = BoolE (n1 > n2)
arith (IntVal n1) GreaterE (IntVal n2) = BoolE (n1 >= n2)
arith (IntVal n1) Equals   (IntVal n2) = BoolE (n1 == n2)

-- | Conditionally choose between two expressions
cond :: Val Bool -> Exp '[] t -> Exp '[] t -> Exp '[] t
cond = undefined

-- | Unroll a `fix` one level
unfix :: Val (ty -> ty) -> Exp '[] ty
unfix (LamVal body) = subst (Fix (Lam body)) body

-- | A well-typed variable in an empty context is impossible.
impossibleVar :: Elem '[] x -> a
impossibleVar _ = error "GHC's typechecker failed"
  -- GHC 7.8+ supports EmptyCase for this, but the warnings for that
  -- construct don't work yet.

-- | Evaluate an expression, using big-step semantics.
eval :: Exp '[] t -> Val t
eval = undefined

-- | Step an expression, either to another expression or to a value.
step :: Exp '[] t -> Either (Exp '[] t) (Val t)
step (Var v)          = impossibleVar v
step (Lam body)       = Right (LamVal body)
step (App e1 e2)      = case step e1 of
                          Left e1' -> Left (App e1' e2)
                          Right lam -> Left (apply lam e2)
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
step (IntE n)         = Right (IntVal n)
step (BoolE b)        = Right (BoolVal b)
