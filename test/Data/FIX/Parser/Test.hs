{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Parser.Test where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(UTCTime))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IntMap

import Data.FIX.Message
import Data.FIX.Parser
import Data.FIX.Test.Common

import qualified Data.FIX.Spec.FIX42 as FIX42

tests :: TestTree
tests = testGroup "Parser"
  [ test_tagP
  , test_tagsP
  , test_groupP
  , test_messageP
  ]

test_tagP :: TestTree
test_tagP = testGroup "tagP unit tests"
  [ testOk  8 "FIX42" (FIXString "FIX42")
  , testOk  9 "15" (FIXInt 15)
  , testOk 35 "D" (FIXString "D")
  , testOk 34 "3" (FIXInt 3)
  , testOk 49 "ABC123" (FIXString "ABC123")
  , testOk 52 "20000101-00:00:00" (FIXTimestamp (UTCTime (fromGregorian 2000 1 1) 0))
  , testOk 56 "Some String" (FIXString "Some String")
  , testOk 10 "255" (FIXInt 255)
  , testOk 40 "1" (FIXChar '1')
  , testOk 44 "-0.35" (FIXDouble (-0.35))
  , testOk 54 "2" (FIXChar '2')
  , testOk 58 "Free form text" (FIXString "Free form text")
  ]
  where
    testOk n txt y =
      let t = fsTags FIX42.fix42 IntMap.! n
      in testCase (printf "%d=%s\\SOH (%s)" n (C.unpack txt) (tName t)) $
           runParser (tagP t) (soh (C.pack (printf "%d=%s" n (C.unpack txt)))) @?= Right y

test_tagsP :: TestTree
test_tagsP = testGroup "tagsP unit tests"
  [ testOk [8, 9, 35] "8=FIX42\SOH9=15\SOH35=D\SOH"
      [FIXString "FIX42", FIXInt 15, FIXString "D"]
  ]
  where
    testOk ns txt ys =
      let ts = IntMap.fromList [(n, fsTags FIX42.fix42 IntMap.! n) | n <- ns]
      in testCase (showLong txt) $
           runParser (fst <$> tagsP ts) txt @?= Right (IntMap.fromList (zip ns ys))

test_groupP :: TestTree
test_groupP = testGroup "groupP unit tests"
  [ testOk (384, 372, []) "1\SOH372=6\SOH"
      [ ((372, FIXString "6"), [])
      ]
  , testOk (384, 372, []) "3\SOH372=6\SOH372=7\SOH372=8\SOH"
      [ ((372, FIXString "6"), [])
      , ((372, FIXString "7"), [])
      , ((372, FIXString "8"), [])
      ]
  , testOk (384, 372, [385]) "2\SOH372=6\SOH385=R\SOH372=7\SOH385=S\SOH"
      [ ((372, FIXString "6"), [(385, FIXChar 'R')]),
        ((372, FIXString "7"), [(385, FIXChar 'S')])
      ]
  ]
  where
    testOk (groupT, sepT, ts) txt gs =
      let tags = fsTags FIX42.fix42
          spec = FGSpec
            (tags IntMap.! groupT)
            (tags IntMap.! sepT)
            (IntMap.fromList [(t, tags IntMap.! t) | t <- ts])
          result = FIXGroup (length gs)
                     [ FIXGroupElement sepT x (IntMap.fromList xs)
                     | ((_, x), xs) <- gs :: [((Int, FIXValue), [(Int, FIXValue)])]]
      in testCase (showLong txt) $
           runParser (groupP spec) txt @?= Right result

test_messageP :: TestTree
test_messageP = testGroup "messageP unit tests"
  [ testOk "D"
      "8=FIX.4.2\SOH9=5\SOH35=D\SOH10=181\SOH"
      [] [] []
  , testOk "R"
      "8=FIX.4.2\SOH9=11\SOH35=R\SOH146=0\SOH10=249\SOH"
      [] [(146,FIXGroup 0 [])] []
  , testOk "D"
      "8=FIX.4.2\SOH9=164\SOH35=D\SOH34=1\SOH49=BLP\SOH50=30737\SOH52=20000809-20:20:50.000\SOH\
      \56=SCHB\SOH97=Y\SOH1=10030003\SOH11=90001008\SOH21=2\SOH38=4000.0\SOH40=2\SOH44=30.0\SOH\
      \47=I\SOH54=1\SOH55=TESTA\SOH59=0\SOH60=20000809-18:20:32.000\SOH10=071\SOH"
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
  , testOk "0"
      "8=FIX.4.2\SOH9=73\SOH35=0\SOH49=BRKR\SOH56=INVMGR\SOH34=235\SOH52=19980604-07:58:28\SOH112=19980604-07:58:28\SOH10=236\SOH"
      -- header
      [ (34,FIXInt 235)
      , (49,FIXString "BRKR")
      , (52,FIXTimestamp (UTCTime (fromGregorian 1998 06 04) 28708))
      , (56,FIXString "INVMGR") ]
      -- body
      [ (112,FIXString "19980604-07:58:28") ]
      -- trailer
      []
  , testOk "0"
      "8=FIX.4.2\SOH9=51\SOH35=0\SOH49=INVMGR\SOH56=BRKR\SOH34=236\SOH52=19980604-07:59:30\SOH10=141\SOH"
      -- header
      [ (34,FIXInt 236)
      , (49,FIXString "INVMGR")
      , (52,FIXTimestamp (UTCTime (fromGregorian 1998 06 04) 28770))
      , (56,FIXString "BRKR") ]
      -- body
      []
      -- trailer
      []
  ]
  where
    testOk msgType txt hdr bdy trl =
      let result = FIXMessage FIX42.fix42 msgType
                     (IntMap.fromList hdr)
                     (IntMap.fromList bdy)
                     (IntMap.fromList trl)
      in testCase (showLong txt) $
           runParser (messageP FIX42.fix42) txt @?= Right result
