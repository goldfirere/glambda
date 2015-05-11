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
import Data.Text

-- | Unchecked expression
data UExp
  = UVar Int   -- ^ de Bruijn index for a variable
  | UGlobal Text
  | ULam Ty UExp
  | UApp UExp UExp
  | UArith UExp UArithOp UExp
  | UCond UExp UExp UExp
  | UIntE Integer
  | UBoolE Bool

instance PrettyExp UExp where
  prettyPrec = pretty_exp

pretty_exp :: Prec -> UExp -> Doc
pretty_exp _    (UVar n)                     = char '#' <> int n
pretty_exp _    (UGlobal n)                  = text (unpack n)
pretty_exp prec (ULam ty body)               = prettyLam prec ty body
pretty_exp prec (UApp e1 e2)                 = prettyApp prec e1 e2
pretty_exp prec (UArith e1 (UArithOp op) e2) = prettyArith prec e1 op e2
pretty_exp prec (UCond e1 e2 e3)             = prettyIf prec e1 e2 e3
pretty_exp _    (UIntE n)                    = integer n
pretty_exp _    (UBoolE True)                = text "true"
pretty_exp _    (UBoolE False)               = text "false"
