{-# LANGUAGE GADTs, PolyKinds #-}
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

module Language.Glambda.Util ( Exists(..), Prec, topPrec, whenM ) where

import Text.Parsec
import Text.PrettyPrint.HughesPJClass

import Control.Applicative
import Control.Monad

instance Pretty ParseError where
  pPrint = text . show

-- | A general-purpose existential wrapper
data Exists t where
  E :: t x -> Exists t

-- | More perspicuous synonym for operator precedence
type Prec = Rational

-- | Precedence for top-level printing
topPrec :: Prec
topPrec = 0

-- | Like 'when', but more monadic
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = flip when thing =<< mb
