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

import Text.PrettyPrint.HughesPJClass

-- | Unchecked expression
data UExp
  = UVar Int   -- ^ de Bruijn index for a variable
  | ULam Ty UExp
  | UApp UExp UExp
  | UArith UExp UArithOp UExp
  | UCond UExp UExp UExp
  | UIntE Integer
  | UBoolE Bool

instance Pretty UExp where
  pPrintPrec _ = pPrint_exp

pPrint_exp :: Prec -> UExp -> Doc
pPrint_exp _    (UVar n)                     = int n
pPrint_exp prec (ULam ty body)               = pPrintLam prec ty body
pPrint_exp prec (UApp e1 e2)                 = pPrintApp prec e1 e2
pPrint_exp prec (UArith e1 (UArithOp op) e2) = pPrintArith prec e1 op e2
pPrint_exp prec (UCond e1 e2 e3)             = pPrintIf prec e1 e2 e3
pPrint_exp _    (UIntE n)                    = integer n
pPrint_exp _    (UBoolE True)                = text "true"
pPrint_exp _    (UBoolE False)               = text "false"
