{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies,
             ScopedTypeVariables, EmptyCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Exp
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- The Exp GADT. Glambda expressions encoded in an 'Exp' value are
-- *always* well-typed.
--
----------------------------------------------------------------------------

module Language.Glambda.Exp (
  Exp(..), Elem(..), Val(..), val, eqExp
  ) where

import Language.Glambda.Pretty
import Language.Glambda.Token
import Language.Glambda.Type
import Language.Glambda.Util

import Text.PrettyPrint.ANSI.Leijen

-- | @Elem xs x@ is evidence that @x@ is in the list @xs@.
data Elem :: [a] -> a -> * where
  EZ :: Elem (x ': xs) x
    -- ^ @EZ :: Elem xs x@ is evidence that @x@ is the first element of @xs@
  ES :: Elem xs x -> Elem (y ': xs) x
    -- ^ @ES ev :: Elem xs x@ is evidence that @x@ is one position later in
    -- @xs@ than is indicated in @ev@

-- | Convert an 'Elem' to a proper deBruijn index
elemToInt :: Elem ctx ty -> Int
elemToInt EZ     = 0
elemToInt (ES e) = 1 + elemToInt e

-- | @Exp ctx ty@ is a well-typed expression of type @ty@ in context
-- @ctx@. Note that a context is a list of types, where a type's index
-- in the list indicates the deBruijn index of the associated term-level
-- variable.
data Exp :: [Ty] -> Ty -> * where
  Var :: Elem ctx ty -> Exp ctx ty
  Lam :: ITy arg => Exp (arg ': ctx) res -> Exp ctx (arg `Arr` res)
  App :: Exp ctx (arg `Arr` res) -> Exp ctx arg -> Exp ctx res
  Arith :: Exp ctx IntTy -> ArithOp ty -> Exp ctx IntTy -> Exp ctx ty
  Cond :: Exp ctx BoolTy -> Exp ctx ty -> Exp ctx ty -> Exp ctx ty
  IntE :: Integer -> Exp ctx IntTy
  BoolE :: Bool -> Exp ctx BoolTy

-- | Well-typed values
data Val :: [Ty] -> Ty -> * where
  IntVal  :: Integer -> Val ctx IntTy
  BoolVal :: Bool -> Val ctx BoolTy
  LamVal  :: ITy arg => Exp (arg ': ctx) res -> Val ctx (arg `Arr` res)

-- | Inject a value back into an expression
val :: Val ctx ty -> Exp ctx ty
val (IntVal n) = IntE n
val (BoolVal n) = BoolE n
val (LamVal body) = Lam body

----------------------------------------------------
-- Equality on expressions
eqExp :: Exp ctx1 ty1 -> Exp ctx2 ty2 -> Bool
eqExp (Var e1) (Var e2) = elemToInt e1 == elemToInt e2
eqExp (Lam body1) (Lam body2) = body1 `eqExp` body2
eqExp (App e1a e1b) (App e2a e2b) = e1a `eqExp` e2a && e1b `eqExp` e2b
eqExp (Arith e1a op1 e1b) (Arith e2a op2 e2b)
  = e1a `eqExp` e2a && op1 `eqArithOp` op2 && e1b `eqExp` e2b
eqExp (Cond e1a e1b e1c) (Cond e2a e2b e2c)
  = e1a `eqExp` e2a && e1b `eqExp` e2b && e1c `eqExp` e2c
eqExp (IntE i1) (IntE i2) = i1 == i2
eqExp (BoolE b1) (BoolE b2) = b1 == b2
eqExp _ _ = False

----------------------------------------------------
-- Pretty-printing

instance PrettyExp (Exp ctx ty) where
  prettyExp = pretty_exp

instance PrettyExp (Val ctx ty) where
  prettyExp coloring prec v = prettyExp coloring prec (val v)

pretty_exp :: Coloring -> Prec -> Exp ctx ty -> Doc
pretty_exp c _    (Var n)                     = prettyVar c (elemToInt n)
pretty_exp c prec (Lam (body :: Exp (arg ': rest) ty))
  = prettyLam c prec (unrefineTy (sty :: STy arg)) body
pretty_exp c prec (App e1 e2)                 = prettyApp c prec e1 e2
pretty_exp c prec (Arith e1 op e2)            = prettyArith c prec e1 op e2
pretty_exp c prec (Cond e1 e2 e3)             = prettyIf c prec e1 e2 e3
pretty_exp _ _    (IntE n)                    = integer n
pretty_exp _ _    (BoolE True)                = text "true"
pretty_exp _ _    (BoolE False)               = text "false"
