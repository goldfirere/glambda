-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Unchecked
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Defines the AST for un-type-checked expressions
--
----------------------------------------------------------------------------

module Language.Glambda.Unchecked ( UExp(..) ) where

import Language.Glambda.Pretty
import Language.Glambda.Type
import Language.Glambda.Token
import Language.Glambda.Util

import Text.PrettyPrint.ANSI.Leijen

-- | Unchecked expression
data UExp
  = UVar Int   -- ^ de Bruijn index for a variable
  | UGlobal String
  | ULam Ty UExp
  | UApp UExp UExp
  | UArith UExp UArithOp UExp
  | UCond UExp UExp UExp
  | UFix UExp
  | UIntE Int
  | UBoolE Bool

instance Pretty UExp where
  pretty = defaultPretty

instance PrettyExp UExp where
  prettyExp = pretty_exp

pretty_exp :: Coloring -> Prec -> UExp -> Doc
pretty_exp c _    (UVar n)                     = prettyVar c n
pretty_exp _ _    (UGlobal n)                  = text n
pretty_exp c prec (ULam ty body)               = prettyLam c prec (Just ty) body
pretty_exp c prec (UApp e1 e2)                 = prettyApp c prec e1 e2
pretty_exp c prec (UArith e1 (UArithOp op) e2) = prettyArith c prec e1 op e2
pretty_exp c prec (UCond e1 e2 e3)             = prettyIf c prec e1 e2 e3
pretty_exp c prec (UFix body)                  = prettyFix c prec body
pretty_exp _ _    (UIntE n)                    = int n
pretty_exp _ _    (UBoolE True)                = text "true"
pretty_exp _ _    (UBoolE False)               = text "false"
