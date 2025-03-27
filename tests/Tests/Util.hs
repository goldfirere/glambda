{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Tests.Util
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Utility definnitions for testing glambda
--
----------------------------------------------------------------------------

module Tests.Util (
  module Test.Tasty,
  testCase,
  (@?=), (@=?), (@?) )
  where

import Language.Glambda.Util

import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?), Assertion )

import Prettyprinter (Pretty, pretty, (<+>), squotes, semi)

import Text.Parsec ( ParseError )

import Data.Function
import Language.Haskell.TH

prettyError :: Pretty a => a -> a -> String
prettyError exp act = render $ pretty "Expected" <+> squotes (pretty exp) <> semi <+>
                                pretty "got" <+> squotes (pretty act)

(@?=) :: (Eq a, Pretty a) => a -> a -> Assertion
act @?= exp = (act == exp) @? prettyError exp act

(@=?) :: (Eq a, Pretty a) => a -> a -> Assertion
exp @=? act = (act == exp) @? prettyError exp act

$( do decs <- reifyInstances ''Eq [ConT ''ParseError]
      case decs of  -- GHC 7.6 eagerly typechecks the instance, sometimes
                    -- reporting a duplicate. Urgh. So we can't quote it.
        [] -> fmap (:[]) $
              instanceD (return []) (appT (conT ''Eq) (conT ''ParseError))
                        [ valD (varP '(==)) (normalB [| (==) `on` show |]) [] ]
        _  -> return [] )

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x)  = pretty "Left" <+> pretty x
  pretty (Right x) = pretty "Right" <+> pretty x
