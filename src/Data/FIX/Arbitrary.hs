-- Module  : Data.FIX.Arbitrary
-- License : LGPL-2.1

{-# LANGUAGE NumDecimals   #-}
{-# LANGUAGE TupleSections #-}

module Data.FIX.Arbitrary
    ( genFIXValues
    , shrinkFIXValues
    , genFIXGroup
    , genFIXMessage
    , genFIXMessage'
    , shrinkFIXMessage
    , genByteString
    , genUTCTime
    , genDay
    , genDiffTime
    , genMonthYear
    )
    where

import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.Functor ((<$>))
import Data.IntMap (IntMap)
import Data.Time.Calendar (Day, addDays, fromGregorian, isLeapYear)
import Data.Time.Clock (DiffTime, UTCTime (..), picosecondsToDiffTime)
import Test.QuickCheck (Gen, arbitrary, shrinkList, suchThat)

import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Test.QuickCheck as QC

import Data.FIX.Message

shrinkIntMap :: IntMap a -> [IntMap a]
shrinkIntMap = map IntMap.fromList . shrinkList (const []) . IntMap.toList

genFIXValues :: IntMap FIXTag -> Gen (IntMap FIXValue)
genFIXValues = fmap IntMap.fromList . traverse f . IntMap.elems
    where
        f tag = (tnum tag,) <$> genValue tag

shrinkFIXValues :: IntMap FIXValue -> [IntMap FIXValue]
shrinkFIXValues = shrinkIntMap

genFIXGroup :: FIXGroupSpec -> Gen FIXValue
genFIXGroup spec = do
    let tagLength = gsLength spec
        tagSep    = gsSeparator spec
        tagBody   = gsBody spec
    let genGroupBody = FIXGroupElement (tnum tagSep)
            <$> genValue tagSep
            <*> genFIXValues tagBody
    t <- genValue tagLength
    case t of
        FIXInt len' -> do
            let len = len' `mod` 4
            FIXGroup len <$> QC.vectorOf len genGroupBody
        _ -> error $ "do not know " ++ show (tnum tagLength)

genFIXMessage :: FIXSpec -> Gen FIXMessage
genFIXMessage spec = do
    msgSpec <- QC.elements (Map.elems (fsMessages spec))
    genFIXMessage' spec msgSpec

genFIXMessage' :: FIXSpec -> FIXMessageSpec -> Gen FIXMessage
genFIXMessage' spec msgSpec = do
    header  <- genFIXValues $ msHeader msgSpec
    body    <- genFIXValues $ msBody msgSpec
    trailer <- genFIXValues $ msTrailer msgSpec
    return FIXMessage
        { mContext = spec
        , mType    = msType msgSpec
        , mHeader  = header
        , mBody    = body
        , mTrailer = trailer }

shrinkFIXMessage :: FIXMessage -> [FIXMessage]
shrinkFIXMessage msg = FIXMessage (mContext msg) (mType msg)
    <$> shrinkIntMap (mHeader  msg)
    <*> shrinkIntMap (mBody    msg)
    <*> shrinkIntMap (mTrailer msg)

--- We generate a random string out of digits and numbers generated string has
--- length at least 1 and most <max>
genByteString :: Gen ByteString
genByteString = do
    l <- QC.choose (1, maxLen)
    C.pack <$> QC.vectorOf l (arbitrary `suchThat` isGoodChar)
    where
        isGoodChar c = isAlphaNum c && isAscii c
        maxLen = 15

genUTCTime :: Gen UTCTime
genUTCTime = UTCTime <$> genDay <*> genDiffTime

genDay :: Gen Day
genDay = do
    year  <- QC.choose (0, 9999)
    let date = fromGregorian year 1 1
    doy <- QC.choose (0, if isLeapYear year then 365 else 364)
    return $ addDays doy date

genMonthYear :: Gen Day
genMonthYear = do
    year  <- QC.choose (0, 9999)
    month <- QC.choose (1, 12)
    return $ fromGregorian year month 1

genDiffTime :: Gen DiffTime
genDiffTime = do
    hour   <- QC.choose (0, 23)
    minute <- QC.choose (0, 59)
    sec    <- QC.choose (0, 59)
    msec   <- QC.choose (0, 999)
    return $ picosecondsToDiffTime
          $ (* 1e9) $ (+ msec)    -- picosecs
          $ (* 1e3) $ (+ sec)     -- msecs
          $ (* 60)  $ (+ minute)
          $ (* 60)  $ hour
