{-# LANGUAGE RankNTypes, DataKinds, PolyKinds, GADTs, RebindableSyntax #-}

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

module Language.Glambda.Check where

import Prelude hiding ( (>>=), (>>), fail, return )

import Language.Glambda.Exp
import Language.Glambda.Unchecked
import Language.Glambda.Util

--import Control.Applicative
--import Control.Monad

------------------------------------------------------
-- Type-checker monad
{-
type TypeError = String

data TcM a = TcError   TypeError
           | TcSuccess a

instance Functor TcM where
  fmap = liftM

instance Applicative TcM where
  pure  = return
  (<*>) = ap

instance Monad TcM where
  return = TcSuccess
  fail   = TcError

  TcError err >>= _ = TcError err
  TcSuccess a >>= f = f a

runTcM :: TcM a -> Either TypeError a
runTcM (TcError err) = Left err
runTcM (TcSuccess x) = Right x
-}
--------------------------------------------------

newtype Cont r e = Cont { runCont :: (forall i. e i -> r) -> r }

(>>=) :: Cont r a -> (forall i. a i -> Cont r b) -> Cont r b
ma >>= fmb
  = Cont $ \k -> runCont ma $ \a -> runCont (fmb a) k

(>>) :: Cont r a -> Cont r b -> Cont r b
ma >> mb
  = Cont $ \k -> runCont ma $ \_ -> runCont mb k

fail :: String -> Cont r a
fail = undefined

return :: e i -> Cont r e
return x = Cont $ \k -> k x

check :: UExp -> (forall t. Exp '[] t -> r) -> r
check uexp k = runCont (go LZ uexp) k
  where
    go :: Length ctx -> UExp -> Cont r (Exp ctx)
    go len_ctx (UVar n) =
      check_var len_ctx n >>= (\elem -> return (Var elem))

    check_var :: Length ctx -> Int -> Cont r (Elem ctx)
    check_var = undefined

{-
check :: Length ctx -> UExp -> Either TypeError (Exists (Exp ctx))
check len_ctx (UVar n) = do
  E elem <- check_var len_ctx n
  return (E $ Var elem)

check_var :: Length ctx -> Int -> Either TypeError (Exists (Elem ctx))
check_var LZ _ = Left "unbound variable"
check_var (LS _) 0 = Right (E EZ)
check_var (LS ctx') n = do
  E elem <- check_var ctx' (n-1)
  return (E $ ES elem)
-}
{-
check :: UExp -> (forall t. Exp '[] t -> r) -> TcM r
check uexp k = go LZ uexp $ return . k
  where
    go :: Length ctx -> UExp -> (forall t. Exp ctx t -> TcM r) -> TcM r
    go len_ctx (UVar n) k = check_var len_ctx n $ \elem -> k (Var elem)
    go len_ctx (ULam ty body) k = go (LS len_ctx) body $ \body' -> k (Lam body')
    go len_ctx (UApp e1 e2) k = go len_ctx e1 $ \e1' -> go len_ctx e2 $ \e2' -> k (App e1' e2')

    check_var :: Length ctx -> Int -> (forall t. Elem ctx t -> TcM r) -> TcM r
    check_var LZ _ _ = fail "unbound variable"
    check_var (LS _) 0 k = k EZ
    check_var (LS l) n k = check_var l (n-1) $ \elem -> k (ES elem)
-}
