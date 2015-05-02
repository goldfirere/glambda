{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Token
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Defines a lexical token
--
----------------------------------------------------------------------------

module Language.Glambda.Token (ArithOp(..), Token(..)) where

import Data.Text                      as Text
import Text.PrettyPrint.HughesPJ      as Pretty
import Text.PrettyPrint.HughesPJClass as Pretty

import Data.List                      as List

data ArithOp = Plus | Minus | Times | Divide | Mod
  deriving Eq

data Token
  = LParen
  | RParen
  | Lambda
  | Arrow
  | DColon
  | ArithOp ArithOp
  | Integer Integer
  | Bool Bool
  | If
  | Then
  | Else
  | Assign
  | Equals
  | Name Text
    deriving Eq

instance Pretty ArithOp where
  pPrint Plus   = char '+'
  pPrint Minus  = char '-'
  pPrint Times  = char '*'
  pPrint Divide = char '/'
  pPrint Mod    = char '%'

instance Pretty Token where
  pPrint       = getDoc . printingInfo
  pPrintList _ = printTogether . List.map printingInfo

type PrintingInfo = (Doc, Bool, Bool)
   -- the bools say whether or not to include a space before or a space after

alone :: Doc -> PrintingInfo
alone = (, True, True)

getDoc :: PrintingInfo -> Doc
getDoc (doc, _, _) = doc

printingInfo :: Token -> PrintingInfo
printingInfo LParen       = (char '(', True, False)
printingInfo RParen       = (char ')', False, True)
printingInfo Lambda       = (char '\\', True, False)
printingInfo Arrow        = alone $ text "->"
printingInfo DColon       = alone $ text "::"
printingInfo (ArithOp a)  = alone $ pPrint a
printingInfo (Integer i)  = alone $ integer i
printingInfo (Bool True)  = alone $ text "true"
printingInfo (Bool False) = alone $ text "false"
printingInfo If           = alone $ text "if"
printingInfo Then         = alone $ text "then"
printingInfo Else         = alone $ text "else"
printingInfo Assign       = alone $ text "="
printingInfo Equals       = alone $ text "=="
printingInfo (Name t)     = alone $ text (unpack t)

printTogether :: [PrintingInfo] -> Doc
printTogether []  = Pretty.empty
printTogether pis = getDoc $ List.foldl1 combine pis
  where
    combine (doc1, before_space, inner_space1) (doc2, inner_space2, after_space)
      | inner_space1 && inner_space2
      = (doc1 <+> doc2, before_space, after_space)

      | otherwise
      = (doc1 <> doc2, before_space, after_space)
