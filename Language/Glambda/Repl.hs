{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Repl
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Implements a REPL for glambda.
--
----------------------------------------------------------------------------

module Language.Glambda.Repl where

import Prelude hiding ( lex )

import Language.Glambda.Lex
import Language.Glambda.Parse
import Language.Glambda.Util

import Text.PrettyPrint.HughesPJClass

import System.Console.Haskeline

import Data.Text
import Language.Haskell.TH.Syntax
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List as List

type Input = InputT IO

main :: IO ()
main = runInputT defaultSettings (helloWorld >> loop)
  where
    loop = do
      m_line <- getInputLine "λ> "
      case List.dropWhile isSpace <$> m_line of
        Nothing -> quit
        Just (':' : cmd) -> whenM (runCommand cmd) loop
        Just expr        -> do outputStrLn (render $ pPrint $ parse <=< lex $ pack expr)
                               loop

-- | Prints welcome message
helloWorld :: Input ()
helloWorld = do
  outputStr lambda
  outputStrLn $ "Welcome to the Glamorous Glambda interpreter, version "
                ++ version ++ "."

-- | The welcome message
lambda :: String
lambda
  = "                   \\\\\\\\\\\\           \n" ++
    "                    \\\\\\\\\\\\          \n" ++
    "                 /-\\ \\\\\\\\\\\\        \n" ++
    "                |   | \\\\\\\\\\\\        \n" ++
    "                 \\-/|  \\\\\\\\\\\\      \n" ++
    "                    | //\\\\\\\\\\\\      \n" ++
    "                 \\-/ ////\\\\\\\\\\\\    \n" ++
    "                    //////\\\\\\\\\\\\    \n" ++
    "                   //////  \\\\\\\\\\\\   \n" ++
    "                  //////    \\\\\\\\\\\\  \n"

-- | The current version of glambda
version :: String
version = $( do Loc { loc_package = pkg_string } <- location
                lift $ case List.stripPrefix "glambda-" pkg_string of
                  Just ver -> ver
                  Nothing  -> "?" )

-------------------------------------------
-- commands

-- | Interpret a command (missing the initial ':'). Returns True when
-- the REPL should continue
runCommand :: String -> Input Bool
runCommand = dispatchCommand cmdTable

type CommandTable = [(String, Input Bool)]

dispatchCommand :: CommandTable -> String -> Input Bool
dispatchCommand table cmd
  = case List.filter ((cmd `List.isPrefixOf`) . fst) table of
      []            -> do outputStrLn $ "Unknown command: '" ++ cmd ++ "'"
                          return True
      [(_, action)] -> action
      many          -> do outputStrLn $ "Ambiguous command: '" ++ cmd ++ "'"
                          outputStrLn $ "Possibilities:"
                          mapM_ (outputStrLn . ("  " ++) . fst) many
                          return True

cmdTable :: CommandTable
cmdTable = [("quit", quit >> return False)]

quit :: Input ()
quit = outputStrLn "Good-bye."
