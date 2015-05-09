{-# LANGUAGE GADTs, OverloadedStrings #-}

module Tests.Check where

import Prelude hiding ( lex )

import Language.Glambda.Exp
import Language.Glambda.Parse
import Language.Glambda.Lex
import Language.Glambda.Check
import Language.Glambda.Type
import Language.Glambda.Eval

import Control.Error

import Data.Text
import Text.PrettyPrint.HughesPJClass

import Data.Functor.Identity
import Data.Maybe
import Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

checkTestCases :: [(Text, Maybe (String, Ty, String))]
checkTestCases = [ ("1", Just ("1", intTy, "1"))
                 , ("1 + true", Nothing)
                 , ("(\\x:Int.x) 5",
                    Just ("(λ#:Int. #0) 5", intTy, "5"))
                 , ("(\\x:Int.\\y:Int->Int.y x) 4 (\\z:Int.z*2)",
                    Just ("(λ#:Int. λ#:Int -> Int. #0 #1) 4 (λ#:Int. #0 * 2)",
                          intTy, "8"))
                 , ("1 + 2 * 3 / 4 - 10 % 3",
                    Just ("1 + 2 * 3 / 4 - 10 % 3", intTy, "1"))
                 , ("if true then 1 else false", Nothing)
                 , ("if 3 - 1 == 2 then \\x:Int.x else \\x:Int.3",
                    Just ("if 3 - 1 == 2 then λ#:Int. #0 else λ#:Int. 3",
                          intTy `Arr` intTy, "λ#:Int. #0"))
                 , ("1 > 2", Just ("1 > 2", boolTy, "false"))
                 , ("2 > 1", Just ("2 > 1", boolTy, "true"))
                 , ("1 > 1", Just ("1 > 1", boolTy, "false"))
                 , ("1 >= 1", Just ("1 >= 1", boolTy, "true"))
                 , ("1 < 2", Just ("1 < 2", boolTy, "true"))
                 , ("1 < 1", Just ("1 < 1", boolTy, "false"))
                 , ("1 <= 1", Just ("1 <= 1", boolTy, "true"))
                 ]

checkTests :: TestTree
checkTests = testGroup "Typechecker" $
  List.map (\(expr_str, m_result) ->
               testCase ("`" ++ unpack expr_str ++ "'") $
               (case runIdentity $ runEitherT $ do
                       uexp <- parse =<< lex expr_str
                       check uexp $ \sty exp -> return $
                         case m_result of
                           Just result
                             -> (render (pPrint exp), unrefineTy sty,
                                 render (pPrint (eval exp)))
                                 @?= result
                           _ -> assertFailure "unexpected type-check success"
                  of
                  Left _  -> assertBool "unexpected failure" (isNothing m_result)
                  Right b -> b)) checkTestCases
