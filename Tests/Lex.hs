{-# LANGUAGE OverloadedStrings #-}

module Tests.Lex where

import Language.Glambda.Lex
import Language.Glambda.Token
import Tests.Util

import Prelude hiding ( lex )

import Data.Text as Text
import Data.List as List

import Test.Tasty
import Test.Tasty.HUnit  ( testCase )

lexTestCases :: [(Text, [Token])]
lexTestCases = [ ("", [])
               , ("  ", [])
               , (" {- hi -}  \n  ", [])
               , (" {----} ", [])
               , (" {- foo {- bar -} blah -}", [])
               , (" {- foo {-- bar -}-}", [])
               , ("{- blah ---}", [])
               , ("{- froggle -} -- blah", [])
               , ("x", [Name "x"])
               , ("(()", [LParen, LParen, RParen])
               , ("++--++", [ArithOp Plus, ArithOp Plus])
               , ("->->", [Arrow, Arrow])
               , ("45+332-89/1*3%xyz", [ Integer 45, ArithOp Plus, Integer 332
                                       , ArithOp Minus, Integer 89, ArithOp Divide
                                       , Integer 1, ArithOp Times, Integer 3
                                       , ArithOp Mod, Name "xyz" ])
               , ("===", [Equals, Assign])
               , ("if x then y else z", [If, Name "x", Then, Name "y", Else, Name "z"])
               , ("ifs trues falsee true-", [ Name "ifs", Name "trues", Name "falsee"
                                            , Bool True, ArithOp Minus ])
               , ("::\\", [DColon, Lambda])
               ]

lexTests :: TestTree
lexTests = testGroup "Lexer" $
  List.map (\(str, out) -> testCase ("`" ++ unpack str ++ "'") $
                           lex str @?= Right out) lexTestCases
