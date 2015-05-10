-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Statement
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Defines the Glambda Statement type, which can either be a bare
-- expression or a global variable assignment.
--
----------------------------------------------------------------------------

module Language.Glambda.Statement ( Statement(..) ) where

import Language.Glambda.Unchecked

import Data.Text
import Text.PrettyPrint.HughesPJClass

-- | A statement can either be a bare expression, which will be evaluated,
-- or an assignment to a global variable.
data Statement = BareExp UExp
               | NewGlobal Text UExp

instance Pretty Statement where
  pPrint (BareExp exp)     = pPrint exp
  pPrint (NewGlobal v exp) = text (unpack v) <+> char '=' <+> pPrint exp
