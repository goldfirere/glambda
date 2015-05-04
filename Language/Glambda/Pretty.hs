{-# LANGUAGE ViewPatterns, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Pretty
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Pretty-printing expressions. This allows reduction of code duplication
-- between unchecked and checked expressions.
--
----------------------------------------------------------------------------

module Language.Glambda.Pretty (
  pPrintLam, pPrintApp, pPrintArith, pPrintIf
  ) where

import Language.Glambda.Token
import Language.Glambda.Type
import Language.Glambda.Util

import Text.PrettyPrint.HughesPJClass

lamPrec, appPrec, appLeftPrec, appRightPrec, ifPrec :: Prec
lamPrec = 1
appPrec = 9
appLeftPrec = 8.9
appRightPrec = 9
ifPrec = 1

opPrec, opLeftPrec, opRightPrec :: ArithOp ty -> Prec
opPrec      (precInfo -> (x, _, _)) = x
opLeftPrec  (precInfo -> (_, x, _)) = x
opRightPrec (precInfo -> (_, _, x)) = x

-- | Returns (overall, left, right) precedences for an 'ArithOp'
precInfo :: ArithOp ty -> (Prec, Prec, Prec)
precInfo Plus     = (5, 4.9, 5)
precInfo Minus    = (5, 4.9, 5)
precInfo Times    = (6, 5.9, 6)
precInfo Divide   = (6, 5.9, 6)
precInfo Mod      = (6, 5.9, 6)
precInfo Less     = (4, 4, 4)
precInfo LessE    = (4, 4, 4)
precInfo Greater  = (4, 4, 4)
precInfo GreaterE = (4, 4, 4)
precInfo Equals   = (4, 4, 4)

-- | Useful shorthand for @pPrintPrec prettyNormal@
ppr :: Pretty exp => Prec -> exp -> Doc
ppr = pPrintPrec prettyNormal

-- | Print a lambda expression
pPrintLam :: Pretty exp => Prec -> Ty -> exp -> Doc
pPrintLam prec ty body
  = maybeParens (prec >= lamPrec) $
    char 'λ' <>
    char '#' <> text ":" <> pPrint ty <>
    char '.' <+> ppr topPrec body

-- | Print an application
pPrintApp :: Pretty exp => Prec -> exp -> exp -> Doc
pPrintApp prec e1 e2
  = maybeParens (prec >= appPrec) $
    ppr appLeftPrec  e1 <+>
    ppr appRightPrec e2

pPrintArith :: Pretty exp => Prec -> exp -> ArithOp ty -> exp -> Doc
pPrintArith prec e1 op e2
  = maybeParens (prec >= opPrec op) $
    ppr (opLeftPrec op) e1 <+>
    pPrint op <+>
    ppr (opRightPrec op) e2

pPrintIf :: Pretty exp => Prec -> exp -> exp -> exp -> Doc
pPrintIf prec e1 e2 e3
  = maybeParens (prec >= ifPrec) $
    hsep [ text "if", pPrint e1, text "then"
         , pPrint e2, text "else", pPrint e3 ]
