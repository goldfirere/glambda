{-# LANGUAGE RankNTypes, DataKinds, PolyKinds, GADTs, FlexibleContexts, CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#ifdef __HADDOCK_VERSION__
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Unchecked
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- The glambda typechecker.
--
----------------------------------------------------------------------------

module Language.Glambda.Check ( check ) where

import Language.Glambda.Exp
import Language.Glambda.Shift
import Language.Glambda.Token
import Language.Glambda.Type
import Language.Glambda.Unchecked
import Language.Glambda.Util
import Language.Glambda.Globals
#ifdef __HADDOCK_VERSION__
import Language.Glambda.Monad ( GlamE )
#endif

import Prettyprinter (Doc, (<+>), squotes, pretty, indent, vcat)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Control.Monad.Reader
import Control.Monad.Except

-- | Abort with a type error in the given expression
typeError :: MonadError (Doc AnsiStyle) m => UExp -> Doc AnsiStyle -> m a
typeError e doc = throwError $
                  doc $$ pretty "in the expression" <+> squotes (prettyT e)

------------------------------------------------
-- The typechecker

-- | Check the given expression, aborting on type errors. The resulting
-- type and checked expression is given to the provided continuation.
-- This is parameterized over the choice of monad in order to support
-- pure operation during testing. 'GlamE' is the canonical choice for the
-- monad.
check :: (MonadError (Doc AnsiStyle) m, MonadReader Globals m)
      => UExp -> (forall t. STy t -> Exp '[] t -> m r)
      -> m r
check = go emptyContext
  where
    go :: (MonadError (Doc AnsiStyle) m, MonadReader Globals m)
       => SCtx ctx -> UExp -> (forall t. STy t -> Exp ctx t -> m r)
       -> m r

    go ctx (UVar n) k
      = check_var ctx n $ \ty elem ->
        k ty (Var elem)

    go ctx (UGlobal n) k
      = do globals <- ask
           lookupGlobal globals n $ \ty exp ->
             k ty (shift_into_ctx ctx exp)

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
               pretty "Bad function application." $$
               indent 2 (vcat [ pretty "Function type:" <+> pretty ty1
                              , pretty "Argument type:" <+> pretty ty2 ])

    go ctx e@(UArith e1 (UArithOp op) e2) k
      = go ctx e1 $ \sty1 e1' ->
        go ctx e2 $ \sty2 e2' ->
        case (sty1, sty2) of
          (SIntTy, SIntTy)
            -> k sty (Arith e1' op e2')
          _ -> typeError e $
               pretty "Bad arith operand(s)." $$
               indent 2 (vcat [ pretty " Left-hand type:" <+> pretty sty1
                              , pretty "Right-hand type:" <+> pretty sty2 ])

    go ctx e@(UCond e1 e2 e3) k
      = go ctx e1 $ \sty1 e1' ->
        go ctx e2 $ \sty2 e2' ->
        go ctx e3 $ \sty3 e3' ->
        case sty1 of
          SBoolTy
            |  Just Refl <- sty2 `eqSTy` sty3
            -> k sty2 (Cond e1' e2' e3')
          _ -> typeError e $
               pretty "Bad conditional." $$
               indent 2 (vcat [ pretty "Flag type:" <+> pretty sty1
                              , squotes (pretty "true") <+> pretty "expression type:"
                                                      <+> pretty sty2
                              , squotes (pretty "false") <+> pretty "expression type:"
                                                       <+> pretty sty3 ])

    go ctx e@(UFix e1) k
      = go ctx e1 $ \sty1 e1' ->
        case sty1 of
          arg `SArr` res
            |  Just Refl <- arg `eqSTy` res
            -> k arg (Fix e1')
          _ -> typeError e $
               pretty "Bad fix over expression with type:" <+> pretty sty1

    go _   (UIntE n)  k = k sty (IntE n)
    go _   (UBoolE b) k = k sty (BoolE b)

check_var :: MonadError (Doc AnsiStyle) m
          => SCtx ctx -> Int
          -> (forall t. STy t -> Elem ctx t -> m r)
          -> m r
check_var SNil           _ _ = throwError (pretty "unbound variable")
                             -- shouldn't happen. caught by parser.

-- | Type-check a de Bruijn index variable
check_var (SCons ty _)   0 k = k ty EZ
check_var (SCons _  ctx) n k = check_var ctx (n-1) $ \ty elem ->
                               k ty (ES elem)

-- | Take a closed expression and shift its indices to make sense in
-- a non-empty context.
shift_into_ctx :: SCtx ctx -> Exp '[] ty -> Exp ctx ty
shift_into_ctx SNil             exp = exp
shift_into_ctx (_ `SCons` ctx') exp = shift $ shift_into_ctx ctx' exp
