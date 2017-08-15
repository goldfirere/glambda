{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies,
             ScopedTypeVariables, TypeFamilyDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Exp
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- The Exp GADT. Glambda expressions encoded in an 'Exp' value are
-- *always* well-typed.
--
----------------------------------------------------------------------------

module Language.Glambda.Exp (
  Exp(..), Elem(..), GlamVal(..), prettyVal, eqExp
  ) where

import Language.Glambda.Pretty
import Language.Glambda.Token
import Language.Glambda.Util
import Language.Glambda.Type

import Text.PrettyPrint.ANSI.Leijen

-- | @Elem xs x@ is evidence that @x@ is in the list @xs@.
-- @EZ :: Elem xs x@ is evidence that @x@ is the first element of @xs@.
-- @ES ev :: Elem xs x@ is evidence that @x@ is one position later in
-- @xs@ than is indicated in @ev@
data Elem :: [a] -> a -> * where
  EZ :: Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

-- | Convert an 'Elem' to a proper de Bruijn index
elemToInt :: Elem ctx ty -> Int
elemToInt EZ     = 0
elemToInt (ES e) = 1 + elemToInt e

-- | @Exp ctx ty@ is a well-typed expression of type @ty@ in context
-- @ctx@. Note that a context is a list of types, where a type's index
-- in the list indicates the de Bruijn index of the associated term-level
-- variable.
data Exp :: [*] -> * -> * where
  Var   :: Elem ctx ty -> Exp ctx ty
  Lam   :: Exp (arg ': ctx) res -> Exp ctx (arg -> res)
  App   :: Exp ctx (arg -> res) -> Exp ctx arg -> Exp ctx res
  Arith :: Exp ctx Int -> ArithOp ty -> Exp ctx Int -> Exp ctx ty
  Cond  :: Exp ctx Bool -> Exp ctx ty -> Exp ctx ty -> Exp ctx ty
  Fix   :: Exp ctx (ty -> ty) -> Exp ctx ty
  IntE  :: Int -> Exp ctx Int
  BoolE :: Bool -> Exp ctx Bool

-- | Classifies types that can be values of glambda expressions
class GlamVal t where
  -- | Well-typed closed values. Encoded as a type family, because
  -- different values have different representations.
  type Val t = r | r -> t

  -- | Convert a glambda value back into a glambda expression
  val :: Val t -> Exp '[] t

instance GlamVal Int where
  type Val Int = Int
  val = IntE

instance GlamVal Bool where
  type Val Bool = Bool
  val = BoolE

instance GlamVal (a -> b) where
  type Val (a -> b) = Exp '[a] b
  val = Lam

----------------------------------------------------
-- | Equality on expressions, needed for testing
eqExp :: Exp ctx1 ty1 -> Exp ctx2 ty2 -> Bool
eqExp (Var e1)      (Var e2)      = elemToInt e1 == elemToInt e2
eqExp (Lam body1)   (Lam body2)   = body1 `eqExp` body2
eqExp (App e1a e1b) (App e2a e2b) = e1a `eqExp` e2a && e1b `eqExp` e2b
eqExp (Arith e1a op1 e1b) (Arith e2a op2 e2b)
  = e1a `eqExp` e2a && op1 `eqArithOp` op2 && e1b `eqExp` e2b
eqExp (Cond e1a e1b e1c) (Cond e2a e2b e2c)
  = e1a `eqExp` e2a && e1b `eqExp` e2b && e1c `eqExp` e2c
eqExp (IntE i1)     (IntE i2)     = i1 == i2
eqExp (BoolE b1)    (BoolE b2)    = b1 == b2
eqExp _             _             = False

----------------------------------------------------
-- Pretty-printing

instance Pretty (Exp ctx ty) where
  pretty = defaultPretty

instance PrettyExp (Exp ctx ty) where
  prettyExp = pretty_exp

-- | Pretty-prints a 'Val'. This needs type information to know how to print.
-- Pattern matching gives GHC enough information to be able to find the
-- 'GlamVal' instance needed to construct the 'PrettyExp' instance.
prettyVal :: Val t -> STy t -> Doc
prettyVal val SIntTy       = pretty (IntE val)
prettyVal val SBoolTy      = pretty (BoolE val)
prettyVal val (_ `SArr` _) = pretty (Lam val)

pretty_exp :: Coloring -> Prec -> Exp ctx ty -> Doc
pretty_exp c _    (Var n)          = prettyVar c (elemToInt n)
pretty_exp c prec (Lam body)       = prettyLam c prec Nothing body
pretty_exp c prec (App e1 e2)      = prettyApp c prec e1 e2
pretty_exp c prec (Arith e1 op e2) = prettyArith c prec e1 op e2
pretty_exp c prec (Cond e1 e2 e3)  = prettyIf c prec e1 e2 e3
pretty_exp c prec (Fix e)          = prettyFix c prec e
pretty_exp _ _    (IntE n)         = int n
pretty_exp _ _    (BoolE True)     = text "true"
pretty_exp _ _    (BoolE False)    = text "false"
