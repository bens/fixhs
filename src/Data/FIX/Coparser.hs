-- Module  : Data.FIX.Coparser
-- License : LGPL-2.1

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.FIX.Coparser
    ( coparse
    ) where

import Data.IntMap (IntMap)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), diffTimeToPicoseconds)
import Prelude hiding (concat, length)

import qualified Data.ByteString.Lazy as BS.Lazy

import Data.FIX.Message
import Data.FIX.Parser (tBeginString, tBodyLength, tCheckSum, tMsgType)

import Data.Coparser (BuilderLike(..), Coparser(..))

import qualified Data.FIX.Message as FIX
import qualified Data.LookupTable as LT

-- implementation is not efficient
-- this is just meant for testing
-- and needs a lot of cleanup
--
-- ideas and issues:
-- a) maybe use some kind of Monoid, resp. Writer, Builder to put together
--    the ByteString. Maybe blazer-builder or Data.Text.Lazy.Builder Monoid
-- b) lazy vs. strict ByteString?
-- c) as for parsing we use a Monad, can we use the dual, a Comonad, here?
-- d) implement Binary for FIXMessage, then call encode to get the ByteString
--    -> seems to be slow (see comments in blaze-builder). also we just need
--       one direction, i.e. put - get would be the FIXParser


instance Coparser (IntMap FIXValue) where
    coparse = pack . _serialize . LT.toList
        where
            _serialize :: [(Int, FIXValue)] -> String
            _serialize = concatMap _serValue

            _serValue :: (Int, FIXValue) -> String
            _serValue (k, FIXGroup i ls) =
                let sub = concatMap coparse ls
                    delim = FIX.delimiter
                in
                    decimal k `append` ('=' `cons` decimal i `append` (delim `cons` sub))

            _serValue (k, v) =
                let delim = FIX.delimiter in
                    decimal k `append` ('=' `cons` coparse v `append` singleton delim)



-- externalize the FIXMessage
instance Coparser FIXMessage where
    coparse m = msg <> chksum
        where
            msg     = header <> msgLen <> (FIX.delimiter `cons` body)
            header  = tag08 <> cons '=' (pack version `snoc` FIX.delimiter)
            msgLen  = tag09 <> cons '=' (decimal $ length body)
            msgType = tag35 <> cons '=' (pack $ unpack $ mType m)
            chksum  = tag10 <> cons '=' (paddedChecksum msg `snoc` FIX.delimiter)

            body = msgType
                <> cons FIX.delimiter (coparse (mHeader m))
                <> coparse (mBody m)
                <> coparse (mTrailer m)

            tag08 = decimal $ tnum tBeginString
            tag09 = decimal $ tnum tBodyLength
            tag35 = decimal $ tnum tMsgType
            tag10 = decimal $ tnum tCheckSum

            version = fsVersion $ mContext m
            paddedChecksum m' = snd (FIX.checksum m') `pad` 3

fromFIXMonthYear :: BuilderLike t => Day -> t
fromFIXMonthYear c =
    let (year, month, _) = toGregorian c
    in (fromIntegral year `pad` 4) <> (month `pad` 2)

fromFIXDateOnly :: BuilderLike t => Day -> t
fromFIXDateOnly c =
    let (year, month, day) = toGregorian c
    in (fromIntegral year `pad` 4) <> (month `pad` 2) <> (day `pad` 2)

fromFIXTimeOnly :: BuilderLike t => DiffTime -> t
fromFIXTimeOnly c =
    let msecs = fromIntegral $ diffTimeToPicoseconds c `div` 1e9
        h  = (msecs `div` (1000 * 60 * 60)) `mod` 24
        m  = (msecs `div` (1000 * 60)) `mod` 60
        s  = (msecs `div`  1000) `mod` 60
        ms = msecs `mod` 1000
        x  = ':' :: Char
        y  = '.' :: Char
    in pad h 2 <> cons x (pad m 2) <> cons x (pad s 2) <> cons y (pad ms 3)

fromFIXTimestamp :: BuilderLike t => UTCTime -> t
fromFIXTimestamp c =
  fromFIXDateOnly (utctDay c) <> ('-' `cons` fromFIXTimeOnly (utctDayTime c))

instance Coparser FIXGroupElement where
    coparse (FIXGroupElement n s vs) =
        decimal n <> cons '=' (coparse s) <> cons FIX.delimiter (coparse vs)

instance Coparser FIXValue where
    coparse (FIXInt a) = decimal a
    coparse (FIXDouble a) = realFloat a
    coparse (FIXChar a) = singleton a
    coparse (FIXBool a)
        | a = singleton 'Y'
        | otherwise = singleton 'N'
    coparse (FIXString a) = pack $ unpack a
    coparse (FIXMultipleValueString a) = pack $ unpack a
    coparse (FIXTimestamp a) = fromFIXTimestamp a
    coparse (FIXTimeOnly a) = fromFIXTimeOnly a
    coparse (FIXDateOnly a) = fromFIXDateOnly a
    coparse (FIXMonthYear a) = fromFIXMonthYear a
    coparse (FIXData a) = pack $ unpack a
    coparse (FIXGroup n ls) =
            decimal n `snoc` FIX.delimiter
        <> concat (map coparse ls)


pad :: BuilderLike a => Int -> Int -> a
pad i w | d <= 0 = decimal i
        | d == 1 = '0' `cons` decimal i
        | otherwise = let prefix = replicate d '0' in
            pack prefix <> decimal i
    where
        d = w - len' i
        len' i' = if i' < 10 then 1 else 1 + len' (i' `div` 10)
