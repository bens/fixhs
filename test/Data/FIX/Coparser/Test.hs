{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Coparser.Test where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(UTCTime))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IntMap

import Data.FIX.Coparser ()
import Data.FIX.Message
import Data.FIX.Test.Common

import qualified Data.Coparser as Co
import qualified Data.FIX.Spec.FIX42 as FIX42

tests :: TestTree
tests = testGroup "Coparser"
  [ test_FIXValue
  , test_FIXMessage
  ]

test_FIXValue :: TestTree
test_FIXValue = testGroup "FIXValue unit tests"
  [ testOk (FIXInt 30) "30"
  , testOk (FIXDouble (-38.06)) "-38.06"
  , testOk (FIXChar '@') "@"
  , testOk (FIXChar '\SOH') "\SOH"
  , testOk (FIXBool True) "Y"
  , testOk (FIXBool False) "N"
  , testOk (FIXString "FIX.4.2") "FIX.4.2"
    -- there's no prefix in the field value, the length is carried in a separate field.
  , testOk (FIXData "123\SOH\NULxyz") "123\SOH\NULxyz"
  , testOk (FIXMultipleValueString "foo bar quux") "foo bar quux"
  , testOk (FIXDateOnly (fromGregorian 2001 1 8)) "20010108"
  , testOk (FIXTimeOnly 3600) "01:00:00.000"
  , testOk (FIXTimestamp (UTCTime (fromGregorian 2001 1 8) 3600)) "20010108-01:00:00.000"
  , testOk (FIXMonthYear (fromGregorian 2001 1 1)) "200101"
  , testOk (FIXGroup 2
              [ FIXGroupElement 372 (FIXString "6") (IntMap.fromList [(385, FIXChar 'R')])
              , FIXGroupElement 372 (FIXString "7") (IntMap.fromList [(385, FIXChar 'S')])
              ])
      "2\SOH372=6\SOH385=R\SOH372=7\SOH385=S\SOH"
  ]
  where
    testOk :: FIXValue -> C.ByteString -> TestTree
    testOk val txt = testCase (showLong val) (Co.coparse val @?= txt)

test_FIXMessage :: TestTree
test_FIXMessage = testGroup "FIXMessage unit tests"
  [ testOk FIX42.fix42 "D"
        [] [] []
      "8=FIX.4.2\SOH9=5\SOH35=D\SOH10=181\SOH"
  , testOk FIX42.fix42 "D"
        -- header
        [ (34,FIXInt 1)
        , (49,FIXString "BLP")
        , (50,FIXString "30737")
        , (52,FIXTimestamp (UTCTime (fromGregorian 2000 8 9) 73250))
        , (56,FIXString "SCHB")
        , (97,FIXBool True)]
        -- body
        [ (1,FIXString "10030003")
        , (11,FIXString "90001008")
        , (21,FIXChar '2')
        , (38,FIXDouble 4000.0)
        , (40,FIXChar '2')
        , (44,FIXDouble 30.0)
        , (47,FIXChar 'I')
        , (54,FIXChar '1')
        , (55,FIXString "TESTA")
        , (59,FIXChar '0')
        , (60,FIXTimestamp (UTCTime (fromGregorian 2000 8 9) 66032)) ]
        -- trailer
        []
    "8=FIX.4.2\SOH9=164\SOH35=D\SOH34=1\SOH49=BLP\SOH50=30737\SOH52=20000809-20:20:50.000\SOH\
    \56=SCHB\SOH97=Y\SOH1=10030003\SOH11=90001008\SOH21=2\SOH38=4000.0\SOH40=2\SOH44=30.0\SOH\
    \47=I\SOH54=1\SOH55=TESTA\SOH59=0\SOH60=20000809-18:20:32.000\SOH10=071\SOH"
  ]
  where
    testOk spec msgType hdr bdy trl txt =
      let msg = FIXMessage spec msgType
                  (IntMap.fromList hdr)
                  (IntMap.fromList bdy)
                  (IntMap.fromList trl)
      in testCase (showLong msg) (Co.coparse msg @?= (txt :: C.ByteString))
