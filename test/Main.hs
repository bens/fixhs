module Main where

import Test.Tasty

import qualified Data.FIX.Coparser.Test as Coparser
import qualified Data.FIX.Parser.Test as Parser
import qualified Data.FIX.ParserCombinators.Test as ParserCombinators
import qualified Data.FIX.Roundtrip.Test as Roundtrip

main :: IO ()
main = defaultMain (testGroup "ALL" testGroups)
  where
    testGroups =
      [ ParserCombinators.tests
      , Parser.tests
      , Coparser.tests
      , Roundtrip.tests
      ]
