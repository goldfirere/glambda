{-# LANGUAGE TupleSections, GADTs, StandaloneDeriving, DataKinds #-}

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

module Language.Glambda.Token (

  -- * Arithmetic operators
  ArithOp(..), UArithOp(..), eqArithOp,

  -- ** Unchecked synonyms for arithmetic operators
  uPlus, uMinus, uTimes, uDivide, uMod, uLess, uLessE,
  uGreater, uGreaterE, uEquals,

  -- * Tokens
  Token(..), LToken(..), unLoc, unArithOp, unInt, unBool, unName
  ) where

import Language.Glambda.Type
import Language.Glambda.Util

import Text.PrettyPrint.ANSI.Leijen  as Pretty
import Text.Parsec.Pos ( SourcePos )

import Data.List                      as List

-- | An @ArithOp ty@ is an operator on numbers that produces a result
-- of type @ty@
data ArithOp ty where
  Plus, Minus, Times, Divide, Mod        :: ArithOp Int
  Less, LessE, Greater, GreaterE, Equals :: ArithOp Bool

-- | 'UArithOp' ("unchecked 'ArithOp'") is an existential package for
-- an 'ArithOp'
data UArithOp where
  UArithOp :: ITy ty => ArithOp ty -> UArithOp

uPlus, uMinus, uTimes, uDivide, uMod, uLess, uLessE, uGreater,
  uGreaterE, uEquals :: UArithOp
uPlus     = UArithOp Plus
uMinus    = UArithOp Minus
uTimes    = UArithOp Times
uDivide   = UArithOp Divide
uMod      = UArithOp Mod
uLess     = UArithOp Less
uLessE    = UArithOp LessE
uGreater  = UArithOp Greater
uGreaterE = UArithOp GreaterE
uEquals   = UArithOp Equals

-- | Compare two 'ArithOp's (potentially of different types) for equality
eqArithOp :: ArithOp ty1 -> ArithOp ty2 -> Bool
eqArithOp Plus     Plus     = True
eqArithOp Minus    Minus    = True
eqArithOp Times    Times    = True
eqArithOp Divide   Divide   = True
eqArithOp Mod      Mod      = True
eqArithOp Less     Less     = True
eqArithOp LessE    LessE    = True
eqArithOp Greater  Greater  = True
eqArithOp GreaterE GreaterE = True
eqArithOp Equals   Equals   = True
eqArithOp _        _        = False

instance Eq (ArithOp ty) where
  (==) = eqArithOp

instance Eq UArithOp where
  UArithOp op1 == UArithOp op2 = op1 `eqArithOp` op2

-- | A lexed token
data Token
  = LParen
  | RParen
  | Lambda
  | Dot
  | Arrow
  | Colon
  | ArithOp UArithOp
  | Int Int
  | Bool Bool
  | If
  | Then
  | Else
  | FixT
  | Assign
  | Semi
  | Name String
    deriving Eq

-- | Perhaps extract a 'UArithOp'
unArithOp :: Token -> Maybe UArithOp
unArithOp (ArithOp x) = Just x
unArithOp _           = Nothing

-- | Perhaps extract an 'Int'
unInt :: Token -> Maybe Int
unInt (Int x) = Just x
unInt _       = Nothing

-- | Perhaps extract an 'Bool'
unBool :: Token -> Maybe Bool
unBool (Bool x) = Just x
unBool _        = Nothing

-- | Perhaps extract a 'String'
unName :: Token -> Maybe String
unName (Name x) = Just x
unName _        = Nothing

-- | A lexed token with location information attached
data LToken = L SourcePos Token

-- | Remove location information from an 'LToken'
unLoc :: LToken -> Token
unLoc (L _ t) = t

instance Pretty (ArithOp ty) where
  pretty Plus     = char '+'
  pretty Minus    = char '-'
  pretty Times    = char '*'
  pretty Divide   = char '/'
  pretty Mod      = char '%'
  pretty Less     = char '<'
  pretty LessE    = text "<="
  pretty Greater  = char '>'
  pretty GreaterE = text ">="
  pretty Equals   = text "=="

instance Show (ArithOp ty) where
  show = render . pretty

instance Pretty UArithOp where
  pretty (UArithOp op) = pretty op

instance Show UArithOp where
  show = render . pretty

instance Pretty Token where
  pretty     = getDoc . printingInfo
  prettyList = printTogether . List.map printingInfo

instance Show Token where
  show = render . pretty

instance Pretty LToken where
  pretty     = pretty . unLoc
  prettyList = prettyList . List.map unLoc

instance Show LToken where
  show = render . pretty

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
printingInfo Dot          = (char '.', False, True)
printingInfo Arrow        = alone $ text "->"
printingInfo Colon        = (char ':', False, False)
printingInfo (ArithOp a)  = alone $ pretty a
printingInfo (Int i)      = alone $ int i
printingInfo (Bool True)  = alone $ text "true"
printingInfo (Bool False) = alone $ text "false"
printingInfo If           = alone $ text "if"
printingInfo Then         = alone $ text "then"
printingInfo Else         = alone $ text "else"
printingInfo FixT         = alone $ text "fix"
printingInfo Assign       = alone $ text "="
printingInfo Semi         = (char ';', False, True)
printingInfo (Name t)     = alone $ text t

printTogether :: [PrintingInfo] -> Doc
printTogether []  = Pretty.empty
printTogether pis = getDoc $ List.foldl1 combine pis
  where
    combine (doc1, before_space, inner_space1) (doc2, inner_space2, after_space)
      | inner_space1 && inner_space2
      = (doc1 <+> doc2, before_space, after_space)

      | otherwise
      = (doc1 <> doc2, before_space, after_space)
