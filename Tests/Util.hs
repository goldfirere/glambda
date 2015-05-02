-----------------------------------------------------------------------------
-- |
-- Module      :  Tests.Util
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Utility definnitions for testing glambda
--
----------------------------------------------------------------------------

module Tests.Util (
  module Test.Tasty,
  testCase,
  (@?=), (@=?) )
  where

import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?), Assertion )

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass

import Text.Parsec ( ParseError )

import Data.Function

prettyError :: Pretty a => a -> a -> String
prettyError exp act = (render $ "Expected" <+> quotes (pPrint exp) <> semi <+>
                                "got" <+> quotes (pPrint act))

(@?=) :: (Eq a, Pretty a) => a -> a -> Assertion
act @?= exp = (act == exp) @? prettyError exp act

(@=?) :: (Eq a, Pretty a) => a -> a -> Assertion
exp @=? act = (act == exp) @? prettyError exp act

instance Eq ParseError where
  (==) = (==) `on` show
