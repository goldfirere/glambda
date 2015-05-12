{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures,
             FlexibleContexts, CPP #-}

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
import Language.Glambda.Util

import System.Console.Haskeline

import Text.PrettyPrint.ANSI.Leijen

import Control.Error
import Control.Monad.Reader
import System.IO

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

-- | A monad giving Haskeline-like interaction and access to 'Globals'
newtype Glam a = Glam { unGlam :: ReaderT Globals (InputT IO) a }
  deriving (Monad, Functor, Applicative, MonadReader Globals)

-- | Like the 'Glam' monad, but also supporting error messages via 'Doc's
newtype GlamE a = GlamE { unGlamE :: EitherT Doc Glam a }
  deriving (Monad, Functor, Applicative, MonadReader Globals, MonadError Doc)

-- | Class for the two glamorous monads
class MonadReader Globals m => GlamM m where
  -- | Print a 'Doc' without a newline at the end
  printDoc :: Doc -> m ()

  -- | Print a 'Doc' with a newline
  printLine :: Doc -> m ()

instance GlamM Glam where
  printDoc = Glam . liftIO . displayIO stdout . toSimpleDoc
  printLine = Glam . liftIO . displayIO stdout . toSimpleDoc . (<> hardline)

instance GlamM GlamE where
  printDoc = GlamE . lift . printDoc
  printLine = GlamE . lift . printLine

-- | Prompt the user for input, returning a string if one is entered.
-- Like 'getInputLine'.
prompt :: String -> Glam (Maybe String)
prompt = Glam . lift . getInputLine

-- | Abort the computation with an error
issueError :: Doc -> GlamE a
issueError = GlamE . throwError

-- | Hoist an 'Either' into 'GlamE'
eitherToGlamE :: Either String a -> GlamE a
eitherToGlamE (Left err) = issueError (text err)
eitherToGlamE (Right x)  = return x

-- | Run a 'Glam' computation
runGlam :: Glam a -> InputT IO a
runGlam thing_inside
  = flip runReaderT emptyGlobals $ unGlam thing_inside

-- | Run a 'GlamE' computation
runGlamE :: GlamE a -> Glam (Either Doc a)
runGlamE thing_inside
  = runEitherT $ unGlamE thing_inside
