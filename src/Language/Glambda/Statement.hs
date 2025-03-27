-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Statement
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Defines the Glambda Statement type, which can either be a bare
-- expression or a global variable assignment.
--
----------------------------------------------------------------------------

module Language.Glambda.Statement ( Statement(..) ) where

import Language.Glambda.Unchecked
import Language.Glambda.Type (prettyT, PrettyT)

import Prettyprinter (Pretty, pretty, (<+>))

-- | A statement can either be a bare expression, which will be evaluated,
-- or an assignment to a global variable.
data Statement = BareExp UExp
               | NewGlobal String UExp

instance PrettyT Statement where
  prettyT (BareExp exp)     = prettyT exp
  prettyT (NewGlobal v exp) = pretty v <+> pretty '=' <+> prettyT exp
