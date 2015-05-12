{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables,
             EmptyCase, DataKinds, TypeFamilies, PolyKinds,
             GADTs #-}

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

module Language.Glambda.Eval ( eval, step, shift ) where

import Language.Glambda.Exp
import Language.Glambda.Token
import Language.Glambda.Type

-- | @Length xs@ tells you how long a list @xs@ is.
-- @LZ :: Length xs@ says that @xs@ is empty.
-- @LS len :: Length xs@ tells you that @xs@ has one more element
-- than @len@ says.
data Length :: [a] -> * where
  LZ :: Length '[]
  LS :: Length xs -> Length (x ': xs)

type family xs ++ ys where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Convert an expression typed in one context to one typed in a larger
-- context. Operationally, this amounts to de Bruijn index shifting.
-- As a proposition, this is the weakening lemma.
shift :: forall ts2 t ty. Exp ts2 ty -> Exp (t ': ts2) ty
shift = go LZ
  where
    go :: forall ts1 ty. Length ts1 -> Exp (ts1 ++ ts2) ty -> Exp (ts1 ++ t ': ts2) ty
    go l_ts1 (Var v)     = Var (shift_elem l_ts1 v)
    go l_ts1 (Lam body)  = Lam (go (LS l_ts1) body)
    go l_ts1 (App e1 e2) = App (go l_ts1 e1) (go l_ts1 e2)
    go l_ts1 (Arith e1 op e2) = Arith (go l_ts1 e1) op (go l_ts1 e2)
    go l_ts1 (Cond e1 e2 e3) = Cond (go l_ts1 e1) (go l_ts1 e2) (go l_ts1 e3)
    go _     (IntE n) = IntE n
    go _     (BoolE b) = BoolE b

    shift_elem :: Length ts1 -> Elem (ts1 ++ ts2) x
               -> Elem (ts1 ++ t ': ts2) x
    shift_elem LZ     e      = ES e
    shift_elem (LS _) EZ     = EZ
    shift_elem (LS l) (ES e) = ES (shift_elem l e)

subst :: forall ts2 s t.
         Exp ts2 s -> Exp (s ': ts2) t -> Exp ts2 t
subst e = go LZ
  where
    go :: forall ts1 t. Length ts1 -> Exp (ts1 ++ s ': ts2) t -> Exp (ts1 ++ ts2) t
    go len (Var v) = subst_var len v
    go len (Lam body) = Lam (go (LS len) body)
    go len (App e1 e2) = App (go len e1) (go len e2)
    go len (Arith e1 op e2) = Arith (go len e1) op (go len e2)
    go len (Cond e1 e2 e3) = Cond (go len e1) (go len e2) (go len e3)
    go _   (IntE n) = IntE n
    go _   (BoolE b) = BoolE b

    subst_var :: forall ts1 t.
                 Length ts1 -> Elem (ts1 ++ s ': ts2) t
              -> Exp (ts1 ++ ts2) t
    subst_var LZ     EZ       = e
    subst_var LZ     (ES v)   = Var v
    subst_var (LS _) EZ       = Var EZ
    subst_var (LS len) (ES v) = shift (subst_var len v)

apply :: Val '[] (arg '`Arr` res) -> Exp '[] arg -> Exp '[] res
apply (LamVal body) arg = subst arg body

arith :: Val '[] IntTy -> ArithOp ty -> Val '[] IntTy -> Val '[] ty
arith (IntVal n1) Plus (IntVal n2) = IntVal (n1 + n2)
arith (IntVal n1) Minus (IntVal n2) = IntVal (n1 - n2)
arith (IntVal n1) Times (IntVal n2) = IntVal (n1 * n2)
arith (IntVal n1) Divide (IntVal n2) = IntVal (n1 `div` n2)
arith (IntVal n1) Mod (IntVal n2) = IntVal (n1 `mod` n2)
arith (IntVal n1) Less (IntVal n2) = BoolVal (n1 < n2)
arith (IntVal n1) LessE (IntVal n2) = BoolVal (n1 <= n2)
arith (IntVal n1) Greater (IntVal n2) = BoolVal (n1 > n2)
arith (IntVal n1) GreaterE (IntVal n2) = BoolVal (n1 >= n2)
arith (IntVal n1) Equals (IntVal n2) = BoolVal (n1 == n2)

cond :: Val '[] BoolTy -> Exp '[] t -> Exp '[] t -> Exp '[] t
cond (BoolVal True)  e _ = e
cond (BoolVal False) _ e = e

-- | Evaluate an expression, using big-step semantics.
eval :: Exp '[] t -> Val '[] t
eval (Var v) = case v of {}
eval (Lam body) = LamVal body
eval (App e1 e2) = eval (apply (eval e1) e2)
eval (Arith e1 op e2) = arith (eval e1) op (eval e2)
eval (Cond e1 e2 e3) = eval (cond (eval e1) e2 e3)
eval (IntE n) = IntVal n
eval (BoolE b) = BoolVal b

-- | Step an expression, either to another expression or to a value.
step :: Exp '[] t -> Either (Exp '[] t) (Val '[] t)
step (Var v) = case v of {}
step (Lam body) = Right (LamVal body)
step (App e1 e2) = case step e1 of
  Left e1' -> Left (App e1' e2)
  Right (LamVal body) -> Left (subst e2 body)
step (Arith e1 op e2) = case step e1 of
  Left e1' -> Left (Arith e1' op e2)
  Right v1 -> case step e2 of
    Left e2' -> Left (Arith (val v1) op e2')
    Right v2 -> Left (val $ arith v1 op v2)
step (Cond e1 e2 e3) = case step e1 of
  Left e1' -> Left (Cond e1' e2 e3)
  Right v1 -> Left (cond v1 e2 e3)
step (IntE n) = Right (IntVal n)
step (BoolE b) = Right (BoolVal b)