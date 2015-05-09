{-# LANGUAGE RankNTypes, DataKinds, PolyKinds, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Unchecked
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- The glambda typechecker
--
----------------------------------------------------------------------------

module Language.Glambda.Check ( check ) where

import Language.Glambda.Exp
import Language.Glambda.Token
import Language.Glambda.Type
import Language.Glambda.Unchecked
import Language.Glambda.Util

import Text.PrettyPrint.HughesPJClass

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Data.Type.Equality

------------------------------------------------------
-- Type-checker monad

type TypeError = String
type TypeT = EitherT TypeError

typeError :: Monad m => UExp -> Doc -> TypeT m a
typeError e doc = throwError $ render $
                  doc $$ text "in the expression" <+> quotes (pPrint e)

------------------------------------------------
-- The typechecker

check :: Monad m => UExp -> (forall t. STy t -> Exp '[] t -> m r)
      -> EitherT String m r
check uexp k = go emptyContext uexp $ \ty expr -> lift (k ty expr)
  where
    go :: Monad m
       => SCtx ctx -> UExp -> (forall t. STy t -> Exp ctx t -> TypeT m r)
       -> TypeT m r

    go ctx (UVar n) k
      = check_var ctx n $ \ty elem ->
        k ty (Var elem)

    go ctx (ULam ty body) k
      = refineTy ty $ \arg_ty ->
        go (arg_ty `SCons` ctx) body $ \res_ty body' ->
        k (arg_ty `SArr` res_ty) (Lam body')

    go ctx e@(UApp e1 e2) k
      = go ctx e1 $ \ty1 e1' ->
        go ctx e2 $ \ty2 e2' ->
        case (ty1, ty2) of
          (SArr arg_ty res_ty, arg_ty')
            |  Just Refl <- arg_ty `eqSTy` arg_ty'
            -> k res_ty (App e1' e2')
          _ -> typeError e $
               hang (text "Bad function application.")
                  2 (vcat [ text "Function type:" <+> pPrint ty1
                          , text "Argument type:" <+> pPrint ty2 ])

    go ctx e@(UArith e1 (UArithOp op) e2) k
      = go ctx e1 $ \sty1 e1' ->
        go ctx e2 $ \sty2 e2' ->
        case (sty1, sty2) of
          (STyCon SIntTc, STyCon SIntTc)
            -> k sty (Arith e1' op e2')
          _ -> typeError e $
               hang (text "Bad arith operand(s).")
                  2 (vcat [ text " Left-hand type:" <+> pPrint sty1
                          , text "Right-hand type:" <+> pPrint sty2 ])

    go ctx e@(UCond e1 e2 e3) k
      = go ctx e1 $ \sty1 e1' ->
        go ctx e2 $ \sty2 e2' ->
        go ctx e3 $ \sty3 e3' ->
        case sty1 of
          STyCon SBoolTc
            |  Just Refl <- sty2 `eqSTy` sty3
            -> k sty2 (Cond e1' e2' e3')
          _ -> typeError e $
               hang (text "Bad conditional.")
                  2 (vcat [ text "Flag type:" <+> pPrint sty1
                          , quotes (text "true") <+> text "expression type:"
                                                 <+> pPrint sty2
                          , quotes (text "false") <+> text "expression type:"
                                                  <+> pPrint sty3 ])

    go _   (UIntE n)  k = k sty (IntE n)
    go _   (UBoolE b) k = k sty (BoolE b)

    check_var :: Monad m
              => SCtx ctx -> Int
              -> (forall t. STy t -> Elem ctx t -> TypeT m r)
              -> TypeT m r
    check_var SNil           _ _ = throwError "unbound variable"
                                 -- shouldn't happen. caught by parser.

    check_var (SCons ty _)   0 k = k ty EZ
    check_var (SCons _  ctx) n k = check_var ctx (n-1) $ \ty elem ->
                                   k ty (ES elem)
