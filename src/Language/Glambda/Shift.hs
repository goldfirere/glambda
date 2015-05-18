{-# LANGUAGE ScopedTypeVariables, DataKinds, PolyKinds, TypeOperators,
             TypeFamilies, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Shift
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- de Bruijn shifting and substitution
--
----------------------------------------------------------------------------

module Language.Glambda.Shift ( shift, subst ) where

import Language.Glambda.Exp

-- | @Length xs@ tells you how long a list @xs@ is.
-- @LZ :: Length xs@ says that @xs@ is empty.
-- @LS len :: Length xs@ tells you that @xs@ has one more element
-- than @len@ says.
data Length :: [a] -> * where
  LZ :: Length '[]
  LS :: Length xs -> Length (x ': xs)

type family (xs :: [a]) ++ (ys :: [a]) :: [a]
type instance '[]       ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Convert an expression typed in one context to one typed in a larger
-- context. Operationally, this amounts to de Bruijn index shifting.
-- As a proposition, this is the weakening lemma.
shift :: forall ts2 t ty. Exp ts2 ty -> Exp (t ': ts2) ty
shift = go LZ
  where
    go :: forall ts1 ty. Length ts1 -> Exp (ts1 ++ ts2) ty -> Exp (ts1 ++ t ': ts2) ty
    go l_ts1 (Var v)          = Var (shift_elem l_ts1 v)
    go l_ts1 (Lam body)       = Lam (go (LS l_ts1) body)
    go l_ts1 (App e1 e2)      = App (go l_ts1 e1) (go l_ts1 e2)
    go l_ts1 (Arith e1 op e2) = Arith (go l_ts1 e1) op (go l_ts1 e2)
    go l_ts1 (Cond e1 e2 e3)  = Cond (go l_ts1 e1) (go l_ts1 e2) (go l_ts1 e3)
    go l_ts1 (Fix e)          = Fix (go l_ts1 e)
    go _     (IntE n)         = IntE n
    go _     (BoolE b)        = BoolE b

    shift_elem :: Length ts1 -> Elem (ts1 ++ ts2) x
               -> Elem (ts1 ++ t ': ts2) x
    shift_elem LZ     e      = ES e
    shift_elem (LS _) EZ     = EZ
    shift_elem (LS l) (ES e) = ES (shift_elem l e)

-- | Substitute the first expression into the second. As a proposition,
-- this is the substitution lemma.
subst :: forall ts2 s t.
         Exp ts2 s -> Exp (s ': ts2) t -> Exp ts2 t
subst e = go LZ
  where
    go :: forall ts1 t. Length ts1 -> Exp (ts1 ++ s ': ts2) t -> Exp (ts1 ++ ts2) t
    go len (Var v)          = subst_var len v
    go len (Lam body)       = Lam (go (LS len) body)
    go len (App e1 e2)      = App (go len e1) (go len e2)
    go len (Arith e1 op e2) = Arith (go len e1) op (go len e2)
    go len (Cond e1 e2 e3)  = Cond (go len e1) (go len e2) (go len e3)
    go len (Fix e)          = Fix (go len e)
    go _   (IntE n)         = IntE n
    go _   (BoolE b)        = BoolE b

    subst_var :: forall ts1 t.
                 Length ts1 -> Elem (ts1 ++ s ': ts2) t
              -> Exp (ts1 ++ ts2) t
    subst_var LZ     EZ       = e
    subst_var LZ     (ES v)   = Var v
    subst_var (LS _) EZ       = Var EZ
    subst_var (LS len) (ES v) = shift (subst_var len v)
