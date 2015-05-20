module Tests.Lex where

import Language.Glambda.Lex
import Language.Glambda.Token
import Tests.Util

import Prelude hiding ( lex )

import Data.List as List
import Control.Arrow as Arrow ( right )

lexTestCases :: [(String, [Token])]
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
               , ("45+332-89/1*3%xyz", [ Int 45, ArithOp uPlus, Int 332
                                       , ArithOp uMinus, Int 89, ArithOp uDivide
                                       , Int 1, ArithOp uTimes, Int 3
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
  List.map (\(str, out) -> testCase ("`" ++ str ++ "'") $
                           Arrow.right (List.map unLoc)
                                        (lex str) @?= Right out)
           lexTestCases
