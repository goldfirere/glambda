{-# LANGUAGE GADTs, PolyKinds, TypeOperators, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Util
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Utility exports (and re-exports) for glambda. This module is meant to
-- be internal -- do not import it if you are not part of the glambda
-- package!
--
----------------------------------------------------------------------------

module Language.Glambda.Util (
  render, toSimpleDoc, maybeParens, ($$),
  Prec, topPrec,
  stripWhitespace, nthDefault,
  (:~:)(..), ignore
  ) where

import Text.Parsec
import Text.PrettyPrint.ANSI.Leijen as Pretty

import Data.Char
import Data.List

#if __GLASGOW_HASKELL__ < 709
import Data.Functor
#endif

#if __GLASGOW_HASKELL__ >= 707
import Data.Type.Equality
#else
data a :~: b where
  Refl :: a :~: a
#endif

-- | Like 'Data.Functor.void'
ignore :: Functor f => f a -> f ()
ignore = (() <$)

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

-- | Synonym for 'Pretty.<$>'
($$) :: Doc -> Doc -> Doc
($$) = (Pretty.<$>)

-- | (Inefficiently) strips whitespace from a string
stripWhitespace :: String -> String
stripWhitespace = dropWhile isSpace . dropWhileEnd isSpace

-- | Pluck out the nth item from a list, or use a default if the list
-- is too short
nthDefault :: a -> Int -> [a] -> a
nthDefault _   0 (x:_)  = x
nthDefault def n (_:xs) = nthDefault def (n-1) xs
nthDefault def _ []     = def
