{-# LANGUAGE GADTs #-}

module Tests.Check where

import Prelude hiding ( lex )

import Language.Glambda.Exp
import Language.Glambda.Parse
import Language.Glambda.Lex
import Language.Glambda.Check
import Language.Glambda.Type
import Language.Glambda.Eval
import Language.Glambda.Globals
import Language.Glambda.Util

import Control.Error
import Control.Monad.Reader

import Text.PrettyPrint.ANSI.Leijen

import Data.List as List
import Control.Arrow as Arrow

import Test.Tasty
import Test.Tasty.HUnit

checkTestCases :: [(String, Maybe (String, Ty, String))]
checkTestCases = [ ("1", Just ("1", IntTy, "1"))
                 , ("1 + true", Nothing)
                 , ("(\\x:Int.x) 5",
                    Just ("(λ#. #0) 5", IntTy, "5"))
                 , ("(\\x:Int.\\y:Int->Int.y x) 4 (\\z:Int.z*2)",
                    Just ("(λ#. λ#. #0 #1) 4 (λ#. #0 * 2)",
                          IntTy, "8"))
                 , ("1 + 2 * 3 / 4 - 10 % 3",
                    Just ("1 + 2 * 3 / 4 - 10 % 3", IntTy, "1"))
                 , ("if true then 1 else false", Nothing)
                 , ("if 3 - 1 == 2 then \\x:Int.x else \\x:Int.3",
                    Just ("if 3 - 1 == 2 then λ#. #0 else λ#. 3",
                          IntTy `Arr` IntTy, "λ#. #0"))
                 , ("1 > 2", Just ("1 > 2", BoolTy, "false"))
                 , ("2 > 1", Just ("2 > 1", BoolTy, "true"))
                 , ("1 > 1", Just ("1 > 1", BoolTy, "false"))
                 , ("1 >= 1", Just ("1 >= 1", BoolTy, "true"))
                 , ("1 < 2", Just ("1 < 2", BoolTy, "true"))
                 , ("1 < 1", Just ("1 < 1", BoolTy, "false"))
                 , ("1 <= 1", Just ("1 <= 1", BoolTy, "true"))
                 , ("id_int (id_int 5)", Just ("(λ#. #0) ((λ#. #0) 5)", IntTy, "5"))
                 ]

checkTests :: TestTree
checkTests = testGroup "Typechecker" $
  List.map (\(expr_str, m_result) ->
               testCase ("`" ++ expr_str ++ "'") $
               (case flip runReader id_globals $ runEitherT $ do
                       uexp <- hoistEither $ Arrow.left text $ parseExp =<< lex expr_str
                       check uexp $ \sty exp -> return $
                         case m_result of
                           Just result
                             -> (render (plain $ pretty exp), unrefineTy sty,
                                 render (plain $ prettyVal (eval exp) sty))
                                 @?= result
                           _ -> assertFailure "unexpected type-check success"
                  of
                  Left _  -> assertBool "unexpected failure" (isNothing m_result)
                  Right b -> b)) checkTestCases

id_globals :: Globals
id_globals = extend "id_int" (SIntTy `SArr` SIntTy) (Lam (Var EZ)) emptyGlobals
