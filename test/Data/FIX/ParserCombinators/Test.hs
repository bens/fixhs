{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.ParserCombinators.Test where

import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (inits)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(UTCTime))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), (@?), testCase)
import Test.Tasty.Hedgehog (testProperty)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as C
import qualified Hedgehog as Hh
import qualified Hedgehog.Gen as Hh
import qualified Hedgehog.Range as Hh

import Data.FIX.ParserCombinators
import Data.FIX.Test.Common

tests :: TestTree
tests = testGroup "ParserCombinators"
    [ test_toInt'
    , prop_toInt'
    , test_toDouble
    , prop_toDouble
    , test_toString
    , test_toDateOnly
    , test_toTimeOnly
    , test_toTimestamp
    , test_toMonthYear
    ]

test_toInt' :: TestTree
test_toInt' = testGroup "toInt' unit tests"
  [ testCase "123456789" (toInt' "123456789" @?= 123456789)
  ]

prop_toInt' :: TestTree
prop_toInt' = testProperty "toInt' via show" $ Hh.property $ do
  x <- Hh.forAll (Hh.integral (Hh.exponential 0 123456789))
  let result = toInt' $ C.pack $ show x
  result Hh.=== x

test_toDouble :: TestTree
test_toDouble = testGroup "toDouble unit tests" $
  [ testOk      "0"         0.0
  , testOk      "0.0"       0.0
  , testOk    "123.123"   123.123
  , testOk "000123.123"   123.123
  , testOk   "+123.123"   123.123
  , testOk   "-123.123" (-123.123)
  , testOk    "456.009"   456.009
  ] ++
  testFails ["", "-", "+", ".", "0.", ".0", "-.0", "+.0"]
  where
    testOk x y = testCase (show x) (runParser toDouble (soh x) @?= Right y)
    testFails xs =
      [ testCase ("ERR " ++ show x) $
            isLeft (runParser toDouble x) @? "Expected parse failure"
      | x <- xs ]

genDouble :: Hh.Gen Double
genDouble = do
  sign <- Hh.element [negate, id]
  n <- Hh.int64 (Hh.exponential 0 (10^(15::Int64)-1))
  d <- Hh.int64 (Hh.linear 0 15)
  let x = fromIntegral (n `div` 10^d)
      y = fromIntegral (n `mod` 10^d) / 10^d
  return (sign $ x + y)

prop_toDouble :: TestTree
prop_toDouble = testProperty "toDouble via show"  $ Hh.property $ do
  x <- Hh.forAll genDouble
  let result = runParser toDouble $ soh (C.pack $ printf "%15.15f" x)
  result Hh.=== Right x

test_toString :: TestTree
test_toString = testGroup "toString unit tests" $
  [ testOk (show x) x x | x <- strings ] ++
  [ testFail "x\SOH"  -- parser will stop before consuming \SOH
  ]
  where
    strings = ["abcd", "!foo0123", "x\NULy"]
    testOk msg x y = testCase msg (runParser toString (soh x) @?= Right y)
    testFail x = testCase ("ERR " ++ show x) $
      isLeft (runParser toString (soh x)) @? "Expected parse failure"

test_toDateOnly :: TestTree
test_toDateOnly = testGroup "toDateOnly unit tests" $
  [ testOk "20000101" (fromGregorian 2000  1  1)
  , testOk "20181123" (fromGregorian 2018 11 23)
  , testOk "19000101" (fromGregorian 1900  1  1)
  , testOk "00000101" (fromGregorian    0  1  1)
  ] ++
  [ testFail (C.pack x) | x <- tail $ inits "2000000" ]
  where
    testOk x y = testCase (show x) (runParser toDateOnly (soh x) @?= Right y)
    testFail x =
      testCase ("ERR " ++ show x)
        (isLeft (runParser toDateOnly (soh x)) @? "Expected parse failure")

test_toTimeOnly :: TestTree
test_toTimeOnly = testGroup "toTimeOnly unit tests" $
  [ testOk "00:00:00"         0
  , testOk "00:00:01"         1
  , testOk "00:01:00"        60
  , testOk "01:00:00"      3600
  , testOk "01:01:01"      3661
  , testOk "10:00:00"     36000
  , testOk "00:00:00.001"     0.001
  , testOk "10:00:00.001" 36000.001
  , testFail "10:00:00."   -- trailing period
  , testFail "30:00:00"    -- hours out of range
  , testFail "00:90:00"    -- minutes out of range
  , testFail "00:00:90"    -- seconds out of range
  ] ++
  [ testFail (C.pack x) | x <- tail $ inits "10:00:0" ]
  where
    testOk x y = testCase (show x) (runParser toTimeOnly (soh x) @?= Right y)
    testFail x =
      testCase ("ERR " ++ show x)
        (isLeft (runParser toTimeOnly (soh x)) @? "Expected parse failure")

test_toTimestamp :: TestTree
test_toTimestamp = testGroup "toTimestamp unit tests" $
  [ testOk   "20000101-00:00:00" (UTCTime (fromGregorian 2000  1  1) 0)
  , testOk   "19210327-01:00:00" (UTCTime (fromGregorian 1921  3 27) 3600)
  ] ++
  [ testFail (C.pack x) | x <- tail $ inits "20000101-00:00:0" ]
  where
    testOk x y = testCase (show x) (runParser toTimestamp (soh x) @?= Right y)
    testFail x =
      testCase ("ERR " ++ show x)
        (isLeft (runParser toTimestamp (soh x)) @? "Expected parse failure")

test_toMonthYear :: TestTree
test_toMonthYear = testGroup "toMonthYear unit tests" $
  [ testOk   "200001" (fromGregorian 2000  1  1)
  , testOk   "192103" (fromGregorian 1921  3  1)
  , testFail "000000" -- month out of range
  , testOk   "000001" (fromGregorian    0  1  1)
  , testFail "999999" -- month out of range
  , testOk   "999912" (fromGregorian 9999 12  1)
  ] ++
  [ testFail (C.pack x) | x <- tail $ inits "20000" ]
  where
    testOk x y = testCase (show x) (runParser toMonthYear (soh x) @?= Right y)
    testFail x =
      testCase ("ERR " ++ show x)
        (isLeft (runParser toMonthYear (soh x)) @? "Expected parse failure")
