{-# LANGUAGE ViewPatterns, GADTs, FlexibleInstances, UndecidableInstances,
             CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}



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
  PrettyExp(..), defaultPretty,
  Coloring, defaultColoring,
  prettyVar, prettyLam, prettyApp, prettyArith, prettyIf, prettyFix
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

-- | A function that changes a 'Doc's color
type ApplyColor = Doc -> Doc

-- | Information about coloring in de Bruijn indexes and binders
data Coloring = Coloring [ApplyColor]
                         [ApplyColor]  -- ^ a stream of remaining colors to use,
                                       -- and the colors used for bound variables

-- | A 'Coloring' for an empty context
defaultColoring :: Coloring
defaultColoring = Coloring all_colors []
  where
    all_colors = red : green : yellow : blue :
                 magenta : cyan : all_colors

-- | A class for expressions that can be pretty-printed
class Pretty exp => PrettyExp exp where
  prettyExp :: Coloring -> Prec -> exp -> Doc

-- | Convenient implementation of 'pretty'
defaultPretty :: PrettyExp exp => exp -> Doc
defaultPretty = nest 2 . prettyExp defaultColoring topPrec

-- | Print a variable
prettyVar :: Coloring -> Int -> Doc
prettyVar (Coloring _ bound) n = (nthDefault id n bound) (char '#' <> int n)

-- | Print a lambda expression
prettyLam :: PrettyExp exp => Coloring -> Prec -> Maybe Ty -> exp -> Doc
prettyLam (Coloring (next : supply) existing) prec m_ty body
  = maybeParens (prec >= lamPrec) $
    fillSep [ char 'λ' <> next (char '#') <>
              maybe empty (\ty -> text ":" <> pretty ty) m_ty <> char '.'
            , prettyExp (Coloring supply (next : existing)) topPrec body ]
prettyLam _ _ _ _ = error "Infinite supply of colors ran out"

-- | Print an application
prettyApp :: (PrettyExp exp1, PrettyExp exp2)
          => Coloring -> Prec -> exp1 -> exp2 -> Doc
prettyApp coloring prec e1 e2
  = maybeParens (prec >= appPrec) $
    fillSep [ prettyExp coloring appLeftPrec  e1
            , prettyExp coloring appRightPrec e2 ]

-- | Print an arithemtic expression
prettyArith :: (PrettyExp exp1, PrettyExp exp2)
            => Coloring -> Prec -> exp1 -> ArithOp ty -> exp2 -> Doc
prettyArith coloring prec e1 op e2
  = maybeParens (prec >= opPrec op) $
    fillSep [ prettyExp coloring (opLeftPrec op) e1 <+> pretty op
            , prettyExp coloring (opRightPrec op) e2 ]

-- | Print a conditional
prettyIf :: (PrettyExp exp1, PrettyExp exp2, PrettyExp exp3)
         => Coloring -> Prec -> exp1 -> exp2 -> exp3 -> Doc
prettyIf coloring prec e1 e2 e3
  = maybeParens (prec >= ifPrec) $
    fillSep [ text "if" <+> prettyExp coloring topPrec e1
            , text "then" <+> prettyExp coloring topPrec e2
            , text "else" <+> prettyExp coloring topPrec e3 ]

-- | Print a @fix@
prettyFix :: PrettyExp exp => Coloring -> Prec -> exp -> Doc
prettyFix coloring prec e
  = maybeParens (prec >= appPrec) $
    text "fix" <+> prettyExp coloring topPrec e
