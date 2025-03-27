{-# LANGUAGE FlexibleInstances,
             UndecidableInstances, CPP, ViewPatterns,
             NondecreasingIndentation #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Repl
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Implements a REPL for glambda.
--
----------------------------------------------------------------------------

module Language.Glambda.Repl ( main ) where

import Prelude hiding ( lex )

import Language.Glambda.Check
import Language.Glambda.Eval
import Language.Glambda.Lex
import Language.Glambda.Parse
import Language.Glambda.Unchecked
import Language.Glambda.Util
import Language.Glambda.Statement
import Language.Glambda.Globals
import Language.Glambda.Monad
import Language.Glambda.Exp
import Language.Glambda.Type

import Prettyprinter (Doc, Pretty, (<+>), pretty, colon, emptyDoc, squotes, parens, vcat, indent)
import Prettyprinter.Render.Terminal (AnsiStyle)

import System.Console.Haskeline
import System.Directory

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List as List

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

-- | The glamorous Glambda interpreter
main :: IO ()
main = runInputT defaultSettings $
       runGlam $ do
         helloWorld
         loop

loop :: Glam ()
loop = do
  m_line <- prompt "λ> "
  case stripWhitespace <$> m_line of
    Nothing          -> quit
    Just (':' : cmd) -> runCommand cmd
    Just str         -> runStmts str
  loop

-- | Prints welcome message
helloWorld :: Glam ()
helloWorld = do
  printLine lambda
  printLine $ pretty "Welcome to the Glamorous Glambda interpreter, version" <+>
              pretty version <> pretty '.'

-- | The welcome message
lambda :: Doc AnsiStyle
lambda
  = vcat $ List.map pretty 
    [ "                   \\\\\\\\\\\\          "
    , "                    \\\\\\\\\\\\         "
    , "                 /-\\ \\\\\\\\\\\\       "
    , "                |   | \\\\\\\\\\\\       "
    , "                 \\-/|  \\\\\\\\\\\\     "
    , "                    | //\\\\\\\\\\\\     "
    , "                 \\-/ ////\\\\\\\\\\\\   "
    , "                    //////\\\\\\\\\\\\   "
    , "                   //////  \\\\\\\\\\\\  "
    , "                  //////    \\\\\\\\\\\\ "
    ]

-- | The current version of glambda
version :: String
version = "1.0"

-------------------------------------------
-- running statements

runStmts :: String -> Glam ()
runStmts str = reportErrors $ do
    toks <- lexG str
    stmts <- parseStmtsG toks
    doStmts stmts

-- | Run a sequence of statements, returning the new global variables
doStmts :: [Statement] -> GlamE Globals
doStmts  = foldr doStmt ask

-- | Run a 'Statement' and then run another action with the global
-- variables built in the 'Statement'
doStmt :: Statement -> GlamE a -> GlamE a
doStmt (BareExp uexp) thing_inside = check uexp $ \sty exp -> do
  printLine $ printValWithType (eval exp) sty
  thing_inside
doStmt (NewGlobal g uexp) thing_inside = check uexp $ \sty exp -> do
  printLine $ pretty g <+> pretty '=' <+> printWithType exp sty
  local (extend g sty exp) thing_inside

-------------------------------------------
-- commands

-- | Interpret a command (missing the initial ':').
runCommand :: String -> Glam ()
runCommand = dispatchCommand cmdTable

type CommandTable = [(String, String -> Glam ())]

dispatchCommand :: CommandTable -> String -> Glam ()
dispatchCommand table line
  = case List.filter ((cmd `List.isPrefixOf`) . fst) table of
      []            -> printLine $ pretty "Unknown command:" <+> squotes (pretty cmd)
      [(_, action)] -> action arg
      many          -> do printLine $ pretty "Ambiguous command:" <+> squotes (pretty cmd)
                          printLine $ pretty "Possibilities:" $$
                                      indent 2 (vcat $ List.map (pretty . fst) many)
  where (cmd, arg) = List.break isSpace line

cmdTable :: CommandTable
cmdTable = [ ("quit",    quitCmd)
           , ("d-lex",   lexCmd)
           , ("d-parse", parseCmd)
           , ("load",    loadCmd)
           , ("eval",    evalCmd)
           , ("step",    stepCmd)
           , ("type",    typeCmd)
           , ("all",     allCmd) ]

quitCmd :: String -> Glam ()
quitCmd _ = quit

class Reportable a where
  report :: a -> Glam Globals

instance Reportable (Doc AnsiStyle) where
  report x = printLine x >> get
instance Reportable () where
  report _ = get
instance Reportable Globals where
  report = return
instance {-# OVERLAPPABLE #-} PrettyT a => Reportable a where
  report other = printLine (prettyT other) >> get

reportErrors :: Reportable a => GlamE a -> Glam ()
reportErrors thing_inside = do
  result <- runGlamE thing_inside
  new_globals <- case result of
    Left err -> printLine err >> get
    Right x  -> report x
  put new_globals

parseLex :: String -> GlamE UExp
parseLex = parseExpG <=< lexG

printWithType :: (PrettyT exp, Pretty ty) => exp -> ty -> Doc AnsiStyle
printWithType exp ty
  = prettyT exp <+> colon <+> pretty ty

printValWithType :: Val ty -> STy ty -> Doc AnsiStyle
printValWithType val sty
  = prettyVal val sty <+> colon <+> pretty sty

lexCmd, parseCmd, evalCmd, stepCmd, typeCmd, allCmd, loadCmd
  :: String -> Glam ()
lexCmd expr = reportErrors $ lexG expr
parseCmd = reportErrors . parseLex

evalCmd expr = reportErrors $ do
  uexp <- parseLex expr
  check uexp $ \sty exp ->
    return $ printValWithType (eval exp) sty

stepCmd expr = reportErrors $ do
  uexp <- parseLex expr
  check uexp $ \sty exp -> do
    printLine $ printWithType exp sty
    let loop e = case step e of
          Left e' -> do
            printLine $ pretty "-->" <+> printWithType e' sty
            loop e'
          Right v -> return v
    v <- loop exp
    return $ printValWithType v sty

typeCmd expr = reportErrors $ do
  uexp <- parseLex expr
  check uexp $ \sty exp -> return (printWithType exp sty)

allCmd expr = do
  printLine (pretty "Small step:")
  _ <- stepCmd expr

  printLine emptyDoc
  printLine (pretty "Big step:")
  evalCmd expr

loadCmd (stripWhitespace -> file) = do
  file_exists <- liftIO $ doesFileExist file
  if not file_exists then file_not_found else do
  contents <- liftIO $ readFile file
  runStmts contents
  where
    file_not_found = do
      printLine (pretty "File not found:" <+> squotes (pretty file))
      cwd <- liftIO getCurrentDirectory
      printLine (parens (pretty "Current directory:" <+> pretty cwd))
