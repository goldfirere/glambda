{-# LANGUAGE OverloadedStrings, FlexibleInstances,
             UndecidableInstances, CPP #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

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

import Text.PrettyPrint.ANSI.Leijen as Pretty hiding ( (<$>) )

import System.Console.Haskeline

import Data.Text
import Control.Monad
import Control.Monad.Reader
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
  where
    loop :: Glam ()
    loop = do
      m_line <- prompt "λ> "
      case List.dropWhile isSpace <$> m_line of
        Nothing -> void (quit "")
        Just (':' : cmd) -> whenM (runCommand cmd) loop
        Just expr        ->
          do result <- runGlamE $ do
               toks <- lexG (pack expr)
               stmt <- parseStmtG toks
               case stmt of
                 BareExp uexp -> check uexp $ \sty exp -> do
                   printLine $ printWithType (eval exp) sty
                   return id
                 NewGlobal g uexp -> check uexp $ \sty exp -> do
                   printLine $
                     text (unpack g) <+> char '=' <+> printWithType exp sty
                   return (extend g sty exp)

             modify_globals <- case result of
               Right gl -> return gl
               Left err -> do
                 printLine err
                 return id

             local modify_globals loop

-- | Prints welcome message
helloWorld :: Glam ()
helloWorld = do
  printLine lambda
  printLine $ text "Welcome to the Glamorous Glambda interpreter, version" <+>
              text version <> char '.'

-- | The welcome message
lambda :: Doc
lambda
  = vcat $ List.map text
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
-- commands

-- | Interpret a command (missing the initial ':'). Returns True when
-- the REPL should continue
runCommand :: String -> Glam Bool
runCommand = dispatchCommand cmdTable

type CommandTable = [(String, String -> Glam Bool)]

dispatchCommand :: CommandTable -> String -> Glam Bool
dispatchCommand table line
  = case List.filter ((cmd `List.isPrefixOf`) . fst) table of
      []            -> do printLine $ text "Unknown command:" <+> squotes (text cmd)
                          return True
      [(_, action)] -> action arg
      many          -> do printLine $ "Ambiguous command:" <+> squotes (text cmd)
                          printLine $ text "Possibilities:" $$
                                      hang 2 (vcat $ List.map (text . fst) many)
                          return True
  where (cmd, arg) = List.break isSpace line

cmdTable :: CommandTable
cmdTable = [ ("quit",  quit)
           , ("lex",   lexCmd)
           , ("parse", parseCmd)
           , ("eval",  evalCmd)
           , ("step",  stepCmd)
           , ("type",  typeCmd)
           , ("all",   allCmd) ]

quit :: String -> Glam Bool
quit _ = do
  printLine (text "Good-bye.")
  return False

class Reportable a where
  report :: a -> Glam ()

instance Reportable Doc where
  report = printLine
instance Reportable () where
  report = return
instance {-# OVERLAPPABLE #-} Pretty a => Reportable a where
  report = printLine . pretty

reportErrors :: Reportable a => GlamE a -> Glam Bool
reportErrors thing_inside = do
  result <- runGlamE thing_inside
  case result of
    Left err -> printLine err
    Right x  -> report x
  return True

parseLex :: String -> GlamE UExp
parseLex = (parseExpG <=< lexG) . pack

printWithType :: (Pretty exp, Pretty ty) => exp -> ty -> Doc
printWithType exp ty
  = pretty exp <+> colon <+> pretty ty

lexCmd, parseCmd, evalCmd, stepCmd, typeCmd, allCmd :: String -> Glam Bool
lexCmd expr = reportErrors $ lexG (pack expr)
parseCmd = reportErrors . parseLex

evalCmd expr = reportErrors $ do
  uexp <- parseLex expr
  check uexp $ \sty exp ->
    return $ printWithType (eval exp) sty

stepCmd expr = reportErrors $ do
  uexp <- parseLex expr
  check uexp $ \sty exp -> do
    printLine $ printWithType exp sty
    let loop e = case step e of
          Left e' -> do
            printLine $ text "-->" <+> printWithType e' sty
            loop e'
          Right v -> return v
    v <- loop exp
    return $ printWithType v sty

typeCmd expr = reportErrors $ do
  uexp <- parseLex expr
  check uexp $ \sty exp -> return (printWithType exp sty)

allCmd expr = do
  printLine (text "Small step:")
  _ <- stepCmd expr

  printLine Pretty.empty
  printLine (text "Big step:")
  _ <- evalCmd expr

  return True
