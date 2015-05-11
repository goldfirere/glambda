{-# LANGUAGE ViewPatterns, GADTs, FlexibleInstances, UndecidableInstances,
             OverlappingInstances #-}

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
  PrettyExp(..), prettyLam, prettyApp, prettyArith, prettyIf
  ) where

import Language.Glambda.Token
import Language.Glambda.Type
import Language.Glambda.Util

import Text.PrettyPrint.ANSI.Leijen

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

class Pretty exp => PrettyExp exp where
  prettyPrec :: Prec -> exp -> Doc

instance PrettyExp exp => Pretty exp where
  pretty = prettyPrec topPrec

-- | Print a lambda expression
prettyLam :: PrettyExp exp => Prec -> Ty -> exp -> Doc
prettyLam prec ty body
  = maybeParens (prec >= lamPrec) $
    char 'λ' <>
    char '#' <> text ":" <> pretty ty <>
    char '.' <+> prettyPrec topPrec body

-- | Print an application
prettyApp :: (PrettyExp exp1, PrettyExp exp2)
          => Prec -> exp1 -> exp2 -> Doc
prettyApp prec e1 e2
  = maybeParens (prec >= appPrec) $
    prettyPrec appLeftPrec  e1 <+>
    prettyPrec appRightPrec e2

prettyArith :: (PrettyExp exp1, PrettyExp exp2)
            => Prec -> exp1 -> ArithOp ty -> exp2 -> Doc
prettyArith prec e1 op e2
  = maybeParens (prec >= opPrec op) $
    prettyPrec (opLeftPrec op) e1 <+>
    pretty op <+>
    prettyPrec (opRightPrec op) e2

prettyIf :: (PrettyExp exp1, PrettyExp exp2, PrettyExp exp3)
         => Prec -> exp1 -> exp2 -> exp3 -> Doc
prettyIf prec e1 e2 e3
  = maybeParens (prec >= ifPrec) $
    hsep [ text "if", pretty e1, text "then"
         , pretty e2, text "else", pretty e3 ]
