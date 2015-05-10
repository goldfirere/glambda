{-# LANGUAGE OverloadedStrings #-}

module Tests.Lex where

import Language.Glambda.Lex
import Language.Glambda.Token
import Tests.Util

import Prelude hiding ( lex )

import Data.Text as Text
import Data.List as List
import Control.Arrow as Arrow ( right )
import Control.Error
import Data.Functor.Identity

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
               , ("++--++", [ArithOp uPlus, ArithOp uPlus])
               , ("->->", [Arrow, Arrow])
               , ("45+332-89/1*3%xyz", [ Integer 45, ArithOp uPlus, Integer 332
                                       , ArithOp uMinus, Integer 89, ArithOp uDivide
                                       , Integer 1, ArithOp uTimes, Integer 3
                                       , ArithOp uMod, Name "xyz" ])
               , ("===", [ArithOp uEquals, Assign])
               , ("if x then y else z", [If, Name "x", Then, Name "y", Else, Name "z"])
               , ("ifs trues falsee true-", [ Name "ifs", Name "trues", Name "falsee"
                                            , Bool True, ArithOp uMinus ])
               , (":\\", [Colon, Lambda])
               , (">>==<===<", [ ArithOp uGreater, ArithOp uGreaterE, Assign
                               , ArithOp uLessE, ArithOp uEquals, ArithOp uLess ])
               ]

lexTests :: TestTree
lexTests = testGroup "Lexer" $
  List.map (\(str, out) -> testCase ("`" ++ unpack str ++ "'") $
                           Arrow.right (List.map unLoc)
                                        (lex str) @?= Right out)
           lexTestCases
