{-# LANGUAGE GADTs, PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}

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

module Language.Glambda.Util (
  render, toSimpleDoc, maybeParens, ($$),
  Prec, topPrec, whenM,
  MonadError(..)
  ) where

import Text.Parsec
import Text.PrettyPrint.ANSI.Leijen as Pretty

import Control.Monad
import Control.Monad.Error

instance Pretty ParseError where
  pretty = text . show

-- | More perspicuous synonym for operator precedence
type Prec = Rational

-- | Precedence for top-level printing
topPrec :: Prec
topPrec = 0

-- | Convert a 'Doc' to a 'String'
render :: Doc -> String
render = flip displayS "" . toSimpleDoc

-- | Convert a 'Doc' to a 'SimpleDoc' for further rendering
toSimpleDoc :: Doc -> SimpleDoc
toSimpleDoc = renderPretty 1.0 78

-- | Enclose a 'Doc' in parens if the flag is 'True'
maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id

-- | Synonym for Pretty.<$>
($$) :: Doc -> Doc -> Doc
($$) = (Pretty.<$>)

-- | Like 'when', but more monadic
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = flip when thing =<< mb
