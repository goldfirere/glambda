-- Main testing module

module Tests.Main where

import Test.Tasty

import Tests.Lex
import Tests.Parse

allTests :: TestTree
allTests = testGroup "Top" [lexTests, parseTests]

main :: IO ()
main = defaultMain allTests
