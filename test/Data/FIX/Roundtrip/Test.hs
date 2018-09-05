{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Roundtrip.Test where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf (printf)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Text.Megaparsec (runParser)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map as Map
import qualified Test.QuickCheck as QC

import Data.FIX.Coparser ()
import Data.FIX.Message
import Data.FIX.Parser (messageP, tagsP)
import Data.FIX.Spec (FIXVersion(..))
import Data.FIX.Split (Error(..), splitting)
import qualified Data.Coparser as Co
import qualified Data.FIX.Arbitrary as Arb
import qualified Data.FIX.Spec.FIX42 as FIX42

tests :: TestTree
tests = testGroup "Roundtrip"
  [ prop_splitting
  , testProperty "FIXValue" prop_FIXValue
  , prop_FIXMessage
  ]

genMessages :: FIXSpec -> FIXMessageSpec -> QC.Gen [FIXMessage]
genMessages fixSpec msgSpec = do
  n <- QC.choose (1,10)
  QC.vectorOf n (Arb.genFIXMessage' fixSpec msgSpec)

shrinkMessages :: [FIXMessage] -> [[FIXMessage]]
shrinkMessages = QC.shrinkList Arb.shrinkFIXMessage

genSegments :: BS.ByteString -> QC.Gen [BS.ByteString]
genSegments = loop
  where
    loop bs
      | BS.null bs = pure []
      | otherwise = do
          n <- QC.choose (1, BS.length bs)
          let (xs, ys) = BS.splitAt n bs
          (:) xs <$> loop ys

shrinkSegments :: [BS.ByteString] -> [[BS.ByteString]]
shrinkSegments [] = []
shrinkSegments as = init (go as)
  where
    go = \case
      [] -> []
      x:y:xs -> ((x <> y):xs) : map (x:) (go (y:xs))
      [x] -> [[x]]

type M = WriterT [BS.Lazy.ByteString] (EitherT Error (State [BS.ByteString]))

prop_splitting :: TestTree
prop_splitting =
  testGroup "Splitting"
    [ testProperty (printf "[%s] %s" (C.unpack $ msType msgSpec) (msName msgSpec)) (f msgSpec)
    | msgSpec <- Map.elems $ fsMessages FIX42.fix42
    ]
  where
    f msgSpec =
      QC.forAllShrink (genMessages FIX42.fix42 msgSpec) shrinkMessages $ \msgs -> do
        let bs :: [BS.ByteString]
            bs = map Co.coparse msgs
        QC.forAllShrink (genSegments (mconcat bs)) shrinkSegments $ \segments ->
          case evalState (runEitherT (execWriterT (splitting FIX42 next failed yield))) segments of
            Right msgs' -> QC.counterexample (show ("EXPECTED"::String, bs)) $
                           QC.counterexample (show ("PARSED"::String,   msgs')) $
                           QC.property (msgs' == map BS.Lazy.fromStrict bs)
            Left err    -> QC.counterexample (show err) $
                           QC.property False
    next :: M BS.ByteString
    next = do
      xs <- lift (lift get)
      case xs of
        a:as -> a <$ lift (lift (put as))
        [] -> pure ""
    failed :: Error -> M ()
    failed err = lift (left err)
    yield :: BS.Lazy.ByteString -> M ()
    yield msg = tell [msg]

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
