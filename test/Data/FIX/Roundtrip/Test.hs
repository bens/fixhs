{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Roundtrip.Test where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf (printf)

import Text.Megaparsec (runParser)

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import qualified Test.QuickCheck as QC

import Data.FIX.Coparser ()
import Data.FIX.Message
import Data.FIX.Parser (messageP, tagsP)
import qualified Data.Coparser as Co
import qualified Data.FIX.Arbitrary as Arb
import qualified Data.FIX.Spec.FIX42 as FIX42

tests :: TestTree
tests = testGroup "Roundtrip"
  [ testProperty "FIXValue" prop_FIXValue
  , prop_FIXMessage
  ]

prop_FIXValue :: QC.Property
prop_FIXValue =
  QC.forAllShrink (Arb.genFIXValues (fsTags FIX42.fix42)) Arb.shrinkFIXValues $ \vs ->
    let bs = Co.coparse vs
        parsed = runParser (tagsP (fsTags FIX42.fix42)) "" bs
    in case parsed of
         Right (vs', _) -> vs == vs'
         Left _err -> False

prop_FIXMessage :: TestTree
prop_FIXMessage =
  testGroup "FIXMessage"
    [ testProperty (printf "[%s] %s" (C.unpack $ msType msgSpec) (msName msgSpec)) (f msgSpec)
    | msgSpec <- Map.elems $ fsMessages FIX42.fix42
    ]
  where
    f msgSpec =
      QC.forAllShrink (Arb.genFIXMessage' FIX42.fix42 msgSpec) Arb.shrinkFIXMessage $ \msg ->
        let bs = Co.coparse msg
            parsed = runParser (messageP FIX42.fix42) "" bs
        in QC.counterexample (show bs) $
           case parsed of
             Right msg' ->
               QC.counterexample (show msg') $
               msg == msg'
             Left err ->
               QC.counterexample ("PARSE FAILURE: " ++ show err) $
               QC.property False
