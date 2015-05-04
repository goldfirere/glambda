{-# LANGUAGE OverloadedStrings, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Type
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Defines types
--
----------------------------------------------------------------------------

module Language.Glambda.Type (
  Ty(..), TyCon(..), readTyCon, IntTy, BoolTy
  ) where

import Language.Glambda.Util

import Text.PrettyPrint.HughesPJClass
import Data.Text

-- | A type constant
data TyCon = IntTc | BoolTc

-- | Perhaps convert a string representation of a base type into a 'TyCon'
readTyCon :: Text -> Maybe TyCon
readTyCon "Int"  = Just IntTc
readTyCon "Bool" = Just BoolTc
readTyCon _      = Nothing

data Ty
  = Arr Ty Ty
  | TyCon TyCon
infixr 1 `Arr`

-- | Convenient synonym for "Int"
type IntTy  = 'TyCon 'IntTc

-- | Convenient synonym for "Bool"
type BoolTy = 'TyCon 'BoolTc

instance Pretty TyCon where
  pPrint IntTc  = text "Int"
  pPrint BoolTc = text "Bool"

instance Show TyCon where
  show = render . pPrint

instance Pretty Ty where
  pPrint = pPrint_ty topPrec

arrowLeftPrec, arrowRightPrec, arrowPrec :: Prec
arrowLeftPrec  = 5
arrowRightPrec = 4.9
arrowPrec      = 5

pPrint_ty :: Prec -> Ty -> Doc
pPrint_ty prec (Arr arg res) = maybeParens (prec >= arrowPrec) $
                               hsep [ pPrint_ty arrowLeftPrec arg
                                    , text "->"
                                    , pPrint_ty arrowRightPrec res ]
pPrint_ty _ (TyCon tc) = pPrint tc
