{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Util
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Utility exports (and re-exports) for glambda.
--
----------------------------------------------------------------------------

module Language.Glambda.Util ( Pretty(..) ) where

import Text.Parsec
import Text.PrettyPrint.HughesPJClass

instance Pretty ParseError where
  pPrint = text . show
