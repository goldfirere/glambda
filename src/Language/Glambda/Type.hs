{-# LANGUAGE DataKinds, TypeOperators, PolyKinds,
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
  Ty(..), readTyCon,

  -- * Glambda types to be used in Haskell types
  STy(..), SCtx(..), ITy(..),
  emptyContext, refineTy, unrefineTy, eqSTy,
  ) where

import Language.Glambda.Util

import Text.PrettyPrint.ANSI.Leijen

-- | Representation of a glambda type
data Ty
  = Arr Ty Ty    -- ^ A function type
  | IntTy
  | BoolTy
  deriving Eq
infixr 1 `Arr`

-- | Perhaps convert a string representation of a base type into a 'Ty'
readTyCon :: String -> Maybe Ty
readTyCon "Int"  = Just IntTy
readTyCon "Bool" = Just BoolTy
readTyCon _      = Nothing

-- | Singleton for a glambda type
data STy :: * -> * where
  SArr    :: STy arg -> STy res -> STy (arg -> res)
  SIntTy  :: STy Int
  SBoolTy :: STy Bool
infixr 1 `SArr`

-- | An implicit 'STy', wrapped up in a class constraint
class ITy ty where
  sty :: STy ty

instance (ITy arg, ITy res) => ITy (arg -> res) where
  sty = sty `SArr` sty
instance ITy Int where
  sty = SIntTy
instance ITy Bool where
  sty = SBoolTy

-- | Singleton for a typing context
data SCtx :: [*] -> * where
  SNil  :: SCtx '[]
  SCons :: STy h -> SCtx t -> SCtx (h ': t)
infixr 5 `SCons`

-- | The singleton for the empty context
emptyContext :: SCtx '[]
emptyContext = SNil

-- | Convert a 'Ty' into an 'STy'.
refineTy :: Ty -> (forall ty. STy ty -> r) -> r
refineTy (ty1 `Arr` ty2) k
  = refineTy ty1 $ \sty1 ->
    refineTy ty2 $ \sty2 ->
    k (sty1 `SArr` sty2)
refineTy IntTy  k = k SIntTy
refineTy BoolTy k = k SBoolTy

-- | Convert an 'STy' into a 'Ty'
unrefineTy :: STy ty -> Ty
unrefineTy (arg `SArr` res) = unrefineTy arg `Arr` unrefineTy res
unrefineTy SIntTy           = IntTy
unrefineTy SBoolTy          = BoolTy

-- | Compare two 'STy's for equality.
eqSTy :: STy ty1 -> STy ty2 -> Maybe (ty1 :~: ty2)
eqSTy (s1 `SArr` t1) (s2 `SArr` t2)
  | Just Refl <- s1 `eqSTy` s2
  , Just Refl <- t1 `eqSTy` t2
  = Just Refl
eqSTy SIntTy  SIntTy  = Just Refl
eqSTy SBoolTy SBoolTy = Just Refl
eqSTy _ _ = Nothing

-----------------------------------------
-- Pretty-printing

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
pretty_ty _ IntTy  = text "Int"
pretty_ty _ BoolTy = text "Bool"
