{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances,
             UndecidableInstances, OverlappingInstances #-}

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

import Language.Glambda.Check
import Language.Glambda.Eval
import Language.Glambda.Lex
import Language.Glambda.Parse
import Language.Glambda.Unchecked
import Language.Glambda.Util

import Text.PrettyPrint.HughesPJClass

import System.Console.Haskeline

import Data.Text
import Language.Haskell.TH.Syntax hiding ( report )
import Control.Applicative
import Control.Monad
import Control.Error
import Data.Char
import Data.List as List

type Input = InputT IO

main :: IO ()
main = runInputT defaultSettings (helloWorld >> loop)
  where
    loop = do
      m_line <- getInputLine "λ> "
      case List.dropWhile isSpace <$> m_line of
        Nothing -> void (quit "")
        Just (':' : cmd) -> whenM (runCommand cmd) loop
        Just expr        ->
          do result <- runEitherT $ do
               toks <- lex (pack expr)
               uexp <- parse toks
               check uexp $ \sty exp -> do
                 outputStrLn $ render $
                   pPrint (eval exp) <+> colon <+> pPrint sty

             case result of
               Right () -> return ()
               Left err -> outputStrLn err

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

type CommandTable = [(String, String -> Input Bool)]

dispatchCommand :: CommandTable -> String -> Input Bool
dispatchCommand table line
  = case List.filter ((cmd `List.isPrefixOf`) . fst) table of
      []            -> do outputStrLn $ "Unknown command: '" ++ cmd ++ "'"
                          return True
      [(_, action)] -> action arg
      many          -> do outputStrLn $ "Ambiguous command: '" ++ cmd ++ "'"
                          outputStrLn $ "Possibilities:"
                          mapM_ (outputStrLn . ("  " ++) . fst) many
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

quit :: String -> Input Bool
quit _ = outputStrLn "Good-bye." >> return False

class Reportable a where
  report :: a -> Input ()

instance Reportable Doc where
  report = outputStrLn . render
instance Reportable () where
  report = return
instance Pretty a => Reportable a where
  report = outputStrLn . render . pPrint

reportEitherT :: Reportable a => EitherT String Input a -> Input Bool
reportEitherT thing_inside = do
  result <- runEitherT thing_inside
  case result of
    Left err      -> outputStrLn err
    Right success -> report success
  return True

parseLex :: String -> EitherT String Input UExp
parseLex = (parse <=< lex) . pack

printWithType :: (Pretty exp, Pretty ty) => exp -> ty -> Doc
printWithType exp ty
  = pPrint exp <+> colon <+> pPrint ty

lexCmd, parseCmd, evalCmd, stepCmd, typeCmd, allCmd :: String -> Input Bool
lexCmd expr = reportEitherT $ lex (pack expr)
parseCmd = reportEitherT . parseLex

evalCmd expr = reportEitherT $ do
  uexp <- parseLex expr
  check uexp $ \sty exp ->
    return $ printWithType (eval exp) sty

stepCmd expr = reportEitherT $ do
  uexp <- parseLex expr
  check uexp $ \sty exp -> do
    outputStrLn $ render $ printWithType exp sty
    let loop e = case step e of
          Left e' -> do
            outputStrLn $ render $ text "-->" <+> printWithType e' sty
            loop e'
          Right v -> return v
    v <- loop exp
    return $ printWithType v sty

typeCmd expr = reportEitherT $ do
  uexp <- parseLex expr
  check uexp $ \sty exp -> return (printWithType exp sty)

allCmd expr = do
  outputStrLn "Small step:"
  _ <- stepCmd expr

  outputStrLn ""
  outputStrLn "Big step:"
  _ <- evalCmd expr

  return True
