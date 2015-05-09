{-# LANGUAGE TupleSections, DeriveDataTypeable, GADTs,
             StandaloneDeriving #-}

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

  -- ** unchecked synonyms for arithmetic operators
  uPlus, uMinus, uTimes, uDivide, uMod, uLess, uLessE,
  uGreater, uGreaterE, uEquals,

  -- * Tokens
  Token(..), LToken(..),
  matchToken, unLoc
  ) where

import Language.Glambda.Type

import Data.Text                      as Text
import Text.PrettyPrint.HughesPJ      as Pretty
import Text.PrettyPrint.HughesPJClass as Pretty
import Text.Parsec.Pos ( SourcePos )

import Data.List                      as List

import Data.Typeable

-- | An @ArithOp ty@ is an operator on numbers that produces a result
-- of type @ty@
data ArithOp ty where
  Plus, Minus, Times, Divide, Mod        :: ArithOp IntTy
  Less, LessE, Greater, GreaterE, Equals :: ArithOp BoolTy
  deriving Typeable

-- | 'UArithOp' ("unchecked 'ArithOp') is an existential package for
-- an 'ArithOp'
data UArithOp where
  UArithOp :: ITy ty => ArithOp ty -> UArithOp
  deriving Typeable

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
  | Integer Integer
  | Bool Bool
  | If
  | Then
  | Else
  | Assign
  | Name Text
    deriving Eq

-- | A lexed token with location information attached
data LToken = L SourcePos Token

unLoc :: LToken -> Token
unLoc (L _ t) = t

-- | Given a constructor and a token, extract the payload of that constructor,
-- if the token matches. For example, @matchToken Integer (Integer 3)@ produces
-- @Just 3@, but @matchToken (\_ -> LParen) RParen@ produces @Nothing@. Make
-- sure the function passed in is /lazy/!
matchToken :: Typeable arg => (arg -> Token) -> Token -> Maybe arg
matchToken fn t
  = case (fn undefined, t) of
      (LParen,    LParen)     -> cast ()
      (RParen,    RParen)     -> cast ()
      (Lambda,    Lambda)     -> cast ()
      (Dot,       Dot)        -> cast ()
      (Arrow,     Arrow)      -> cast ()
      (Colon,     Colon)      -> cast ()
      (ArithOp _, ArithOp op) -> cast op
      (Integer _, Integer n)  -> cast n
      (Bool _,    Bool b)     -> cast b
      (If,        If)         -> cast ()
      (Then,      Then)       -> cast ()
      (Else,      Else)       -> cast ()
      (Assign,    Assign)     -> cast ()
      (Name _,    Name t)     -> cast t
      _                       -> Nothing

instance Pretty (ArithOp ty) where
  pPrint Plus     = char '+'
  pPrint Minus    = char '-'
  pPrint Times    = char '*'
  pPrint Divide   = char '/'
  pPrint Mod      = char '%'
  pPrint Less     = char '<'
  pPrint LessE    = text "<="
  pPrint Greater  = char '>'
  pPrint GreaterE = text ">="
  pPrint Equals   = text "=="

instance Show (ArithOp ty) where
  show = render . pPrint

instance Pretty UArithOp where
  pPrint (UArithOp op) = pPrint op

instance Show UArithOp where
  show = render . pPrint

instance Pretty Token where
  pPrint       = getDoc . printingInfo
  pPrintList _ = printTogether . List.map printingInfo

instance Show Token where
  show = render . pPrint

instance Pretty LToken where
  pPrint       = pPrint . unLoc
  pPrintList p = pPrintList p . List.map unLoc

instance Show LToken where
  show = render . pPrint

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
printingInfo (ArithOp a)  = alone $ pPrint a
printingInfo (Integer i)  = alone $ integer i
printingInfo (Bool True)  = alone $ text "true"
printingInfo (Bool False) = alone $ text "false"
printingInfo If           = alone $ text "if"
printingInfo Then         = alone $ text "then"
printingInfo Else         = alone $ text "else"
printingInfo Assign       = alone $ text "="
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
