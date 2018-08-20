-- Module  : Data.FIX.Arbitrary
-- License : LGPL-2.1

module Data.FIX.Arbitrary
    ( arbitraryFIXValues
    , arbitraryFIXGroup
    , arbitraryFIXMessage
    , arbitraryByteString
    , arbitraryUTCTime
    , arbitraryDay
    , arbitraryDiffTime
    , arbitraryMonthYear
    )
    where

import Data.FIX.Message (
    FIXGroupElement(..), FIXTag(..), FIXValue(..), FIXValues, FIXTags
      , FIXMessage(..), FIXSpec, FIXMessageSpec(..), FIXGroupSpec(..) )
import Data.Time.Calendar ( Day, fromGregorian )
import Data.Time.Clock ( DiffTime, UTCTime (..), picosecondsToDiffTime )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C ( pack )
import qualified Data.Char as C ( isAscii, isAlphaNum )
import qualified Data.LookupTable as LT ( toList, fromList )
import Data.Functor ( (<$>) )
import Control.Monad ( replicateM )
import Test.QuickCheck ( Gen, arbitrary )

arbitraryFIXValues :: FIXTags -> Gen FIXValues
arbitraryFIXValues tags =
    let tlist :: [FIXTag]
        tlist = map snd $ LT.toList tags
        arb :: FIXTag -> Gen (Int, FIXValue)
        arb tag = (,) (tnum tag) <$> arbitraryValue tag
    in
        LT.fromList <$> mapM arb tlist

arbitraryFIXGroup :: FIXGroupSpec -> Gen FIXValue
arbitraryFIXGroup spec =
    let ltag = gsLength spec in do
       t <- arbitraryValue ltag
       case t of
        FIXInt l' -> let l = l' `mod` 4 in
           do bodies <- replicateM l arbitraryGBody
              return $ FIXGroup l bodies
        _         -> error $ "do not know " ++ show (tnum ltag)
    where
        arbitraryGBody =
           let stag = gsSeperator spec
               btags = gsBody spec
           in do
               s  <- arbitraryValue stag
               vs <- arbitraryFIXValues btags
               return (FIXGroupElement (tnum stag) s vs)

arbitraryFIXMessage :: FIXSpec -> FIXMessageSpec -> Gen (FIXMessage FIXSpec)
arbitraryFIXMessage context spec = do
    header <- arbitraryFIXValues $ msHeader spec
    body <- arbitraryFIXValues $ msBody spec
    trailer <- arbitraryFIXValues $ msTrailer spec
    return FIXMessage
        { mContext = context
        , mType = msType spec
        , mHeader = header
        , mBody = body
        , mTrailer = trailer }

-- An arbitrary instance of ByteString.
--- we generate a random string out of digits and numbers
--- generated string has length at least 1 and most <max>
arbitraryByteString :: Gen ByteString
arbitraryByteString = do
    l' <- arbitrary :: Gen Int
    let l = 1 + l' `mod` maxLen
    C.pack <$> replicateM l (aChar isAlpha')
    where
        aChar :: (Char -> Bool) -- predicate
                -> Gen Char     -- random generator
        aChar p = do
            c <- arbitrary
            if p c then return c else aChar p

        isAlpha' c = C.isAlphaNum c && C.isAscii c
        maxLen = 15

arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = UTCTime <$> arbitraryDay <*> arbitraryDiffTime

arbitraryDay :: Gen Day
arbitraryDay = do
    year  <- (`mod` 10000) <$> arbitrary
    month <- (`mod` 12) <$> arbitrary
    day   <- (`mod` 28) <$> arbitrary
    return $ fromGregorian year month day

arbitraryMonthYear :: Gen Day
arbitraryMonthYear = do
    year  <- (`mod` 10000) <$> arbitrary
    month <- (`mod` 12) <$> arbitrary
    return $ fromGregorian year month 1

arbitraryDiffTime :: Gen DiffTime
arbitraryDiffTime = do
    hour   <- (`mod` 24) <$> arbitrary
    minute <- (`mod` 60) <$> arbitrary
    sec    <- (`mod` 60) <$> arbitrary
    psec   <- (`mod` 1000000000000) <$> arbitrary
    return $ picosecondsToDiffTime
          $ (+ psec)
          $ (* 1000000)            -- picosecs
          $ (* 1000) $ (+ sec)     -- msecs
          $ (* 60)   $ (+ minute)
          $ (* 60)   $ hour
