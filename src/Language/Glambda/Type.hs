{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, PolyKinds,
             GADTs, RankNTypes, FlexibleInstances #-}

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
  -- * Glambda types to be used in Haskell terms
  Ty(..), TyCon(..), readTyCon, intTy, boolTy,

  -- * Glambda types to be used in Haskell types
  STyCon(..), STy(..), SCtx(..), ITy(..),
  emptyContext, refineTy, unrefineTy, eqSTy,
  IntTy, BoolTy, sIntTy, sBoolTy
  ) where

import Language.Glambda.Util

import Text.PrettyPrint.ANSI.Leijen
import Data.Type.Equality
import Data.Text

-- | A type constant
data TyCon = IntTc | BoolTc
  deriving Eq

-- | Singleton for a type constant
data STyCon :: TyCon -> * where
  SIntTc  :: STyCon 'IntTc
  SBoolTc :: STyCon 'BoolTc

-- | Perhaps convert a string representation of a base type into a 'TyCon'
readTyCon :: Text -> Maybe TyCon
readTyCon "Int"  = Just IntTc
readTyCon "Bool" = Just BoolTc
readTyCon _      = Nothing

-- | Representation of a glambda type
data Ty
  = Arr Ty Ty    -- ^ A function type
  | TyCon TyCon  -- ^ A type constant
  deriving Eq
infixr 1 `Arr`

-- | Convenient synonym for "Int"
type IntTy  = 'TyCon 'IntTc

-- | Convenient synonym for "Int"
intTy :: Ty
intTy = TyCon IntTc

-- | Convenient synonym for "Int"
sIntTy :: STy IntTy
sIntTy = STyCon SIntTc

-- | Convenient synonym for "Bool"
type BoolTy = 'TyCon 'BoolTc

-- | Convenient synonym for "Bool"
boolTy :: Ty
boolTy = TyCon BoolTc

-- | Convenient synonym for "Bool"
sBoolTy :: STy BoolTy
sBoolTy = STyCon SBoolTc

-- | Singleton for a glambda type
data STy :: Ty -> * where
  SArr   :: STy arg -> STy res -> STy (arg `Arr` res)
  STyCon :: STyCon tc -> STy ('TyCon tc)
infixr 1 `SArr`

-- | An implicit 'STy', wrapped up in a class constraint
class ITy (ty :: Ty) where
  sty :: STy ty

instance (ITy arg, ITy res) => ITy (arg `Arr` res) where
  sty = sty `SArr` sty
instance ITy ('TyCon 'IntTc) where
  sty = STyCon SIntTc
instance ITy ('TyCon 'BoolTc) where
  sty = STyCon SBoolTc

-- | Singleton for a typing context
data SCtx :: [Ty] -> * where
  SNil  :: SCtx '[]
  SCons :: STy h -> SCtx t -> SCtx (h ': t)
infixr 5 `SCons`

emptyContext :: SCtx '[]
emptyContext = SNil

refineTy :: Ty -> (forall ty. ITy ty => STy ty -> r) -> r
refineTy (ty1 `Arr` ty2) k
  = refineTy ty1 $ \sty1 ->
    refineTy ty2 $ \sty2 ->
    k (sty1 `SArr` sty2)
refineTy (TyCon IntTc)  k = k (STyCon SIntTc)
refineTy (TyCon BoolTc) k = k (STyCon SBoolTc)

unrefineTy :: STy ty -> Ty
unrefineTy (arg `SArr` res) = unrefineTy arg `Arr` unrefineTy res
unrefineTy (STyCon SIntTc)  = TyCon IntTc
unrefineTy (STyCon SBoolTc) = TyCon BoolTc

eqSTy :: STy ty1 -> STy ty2 -> Maybe (ty1 :~: ty2)
eqSTy (s1 `SArr` t1) (s2 `SArr` t2)
  | Just Refl <- s1 `eqSTy` s2
  , Just Refl <- t1 `eqSTy` t2
  = Just Refl
eqSTy (STyCon SIntTc)  (STyCon SIntTc)  = Just Refl
eqSTy (STyCon SBoolTc) (STyCon SBoolTc) = Just Refl
eqSTy _ _ = Nothing

-----------------------------------------
-- Pretty-printing

instance Pretty TyCon where
  pretty IntTc  = text "Int"
  pretty BoolTc = text "Bool"

instance Show TyCon where
  show = render . pretty

instance Pretty Ty where
  pretty = pretty_ty topPrec

instance Show Ty where
  show = render . pretty

instance Pretty (STy ty) where
  pretty = pretty . unrefineTy

arrowLeftPrec, arrowRightPrec, arrowPrec :: Prec
arrowLeftPrec  = 5
arrowRightPrec = 4.9
arrowPrec      = 5

pretty_ty :: Prec -> Ty -> Doc
pretty_ty prec (Arr arg res) = maybeParens (prec >= arrowPrec) $
                               hsep [ pretty_ty arrowLeftPrec arg
                                    , text "->"
                                    , pretty_ty arrowRightPrec res ]
pretty_ty _ (TyCon tc) = pretty tc
