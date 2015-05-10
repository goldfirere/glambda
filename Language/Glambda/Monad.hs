{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures,
             FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Monad
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- The Glam monad, allowing for pretty-printed output to the user, failing
-- with an error message, and tracking global variables.
--
----------------------------------------------------------------------------

module Language.Glambda.Monad (
  Glam, runGlam, prompt, GlamE, runGlamE, issueError, eitherToGlamE,
  GlamM(..)
  ) where

import Language.Glambda.Globals

import System.Console.Haskeline

import Text.PrettyPrint.HughesPJClass

import Control.Error
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative

newtype Glam a = Glam { unGlam :: ReaderT Globals (InputT IO) a }
  deriving (Monad, Functor, Applicative, MonadReader Globals)

newtype GlamE a = GlamE { unGlamE :: EitherT Doc Glam a }
  deriving (Monad, Functor, Applicative, MonadReader Globals)

class GlamM m where
  printDoc :: Doc -> m ()
  printLine :: Doc -> m ()
  globals  :: m Globals
  modifyGlobals :: (Globals -> Globals) -> m a -> m a

  default globals :: MonadReader Globals m => m Globals
  globals = ask

  default modifyGlobals :: MonadReader Globals m
                        => (Globals -> Globals) -> m a -> m a
  modifyGlobals = local

instance GlamM Glam where
  printDoc = Glam . lift . outputStr . render
  printLine = Glam . lift . outputStrLn . render

instance GlamM GlamE where
  printDoc = GlamE . lift . printDoc
  printLine = GlamE . lift . printLine

prompt :: String -> Glam (Maybe String)
prompt = Glam . lift . getInputLine

issueError :: Doc -> GlamE a
issueError = GlamE . throwError

eitherToGlamE :: Either String a -> GlamE a
eitherToGlamE (Left err) = issueError (text err)
eitherToGlamE (Right x)  = return x

runGlam :: Glam a -> InputT IO a
runGlam thing_inside
  = flip runReaderT emptyGlobals $ unGlam thing_inside

runGlamE :: GlamE a -> Glam (Either Doc a)
runGlamE thing_inside
  = runEitherT $ unGlamE thing_inside
