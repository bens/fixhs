-- Module  : Data.FIX.ParserCombinators
-- License : LGPL-2.1

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumDecimals  #-}

module Data.FIX.ParserCombinators
    ( toTag
    , toString
    , toInt'
    , toInt
    , toDouble
    , toBool
    , toTimestamp
    , toDateOnly
    , toMonthYear
    , toTimeOnly
    , toChar
    , skipToken
    , delimited

    -- exporting Attoparsec
    , Atto.Parser, Zepto.parse
    ) where

import Prelude hiding (null, tail, head)

import Control.Applicative ((<$>), (<|>))
import Control.Monad (guard, join, replicateM)
import Data.Attoparsec.ByteString.Char8 ((<?>))
import Data.Bits ((.&.))
import Data.Word (Word8)
import Data.Char (ord)
import Data.List (foldl')
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (DiffTime, UTCTime (..), picosecondsToDiffTime)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Attoparsec.Zepto as Zepto
import qualified Data.ByteString as BS
import qualified Data.FIX.Common as FIX

delimited :: Atto.Parser a -> Atto.Parser a
delimited m = m <* (Atto.char8 FIX.delimiter <?> "delimiter")

toDouble :: Atto.Parser Double
toDouble = (<?> "float value") $ delimited $ Atto.signed $ do
    a <- Atto.decimal :: Atto.Parser Integer
    (!m, !e) <- join (((extract_decimals <$> Atto.takeWhile1 (/= FIX.delimiter)) <$ Atto.char '.') <|> pure (pure (0, 1)))
    return $! fromIntegral a + fromIntegral m / (10^e)
    where
        extract_decimals :: BS.ByteString -> (Int, Int)
        extract_decimals = BS.foldl' helper (0, 0)
            where
                helper (!m, !e) c = (m * 10 + w2d c, succ e)

parseIntTill :: Char -> Atto.Parser Int
parseIntTill c = Atto.signed Atto.decimal <* Atto.char8 c

toInt' :: BS.ByteString -> Int
toInt' = BS.foldl' (\r c -> r * 10 + w2d c) 0

toInt :: Atto.Parser Int
toInt = delimited (Atto.signed Atto.decimal) <?> "int value"

toChar :: Atto.Parser Char
toChar = delimited Atto.anyChar <?> "char value"

toString :: Atto.Parser BS.ByteString
toString = delimited (Atto.takeWhile1 (/= FIX.delimiter)) <?> "string value"

toTag :: Atto.Parser Int
toTag = parseIntTill '=' <?> "FIX tag"

toBool :: Atto.Parser Bool
toBool = (<?> "boolean") $ delimited (f <$> p)
    where
        p = Atto.char 'Y' <|> Atto.char 'N'
        f 'Y' = True
        f 'N' = False
        f _   = error "wrong boolean FIX value"

parseDate :: Atto.Parser Day
parseDate = do
  year <- (fromIntegral <$> nDigits 4) <?> "year"
  mon  <- nDigits 2 <?> "month"
  day  <- nDigits 2 <?> "day"
  maybe (fail "invalid date") return $ fromGregorianValid year mon day

parseTimeOfDay :: Atto.Parser DiffTime
parseTimeOfDay = do
  hours  <- nDigits 2 <* Atto.char ':' <?> "hour"
  guard (hours < 24) <?> "hour between 0 and 23"
  mins   <- nDigits 2 <* Atto.char ':' <?> "minute"
  guard (mins < 60) <?> "minute between 0 and 59"
  sec    <- nDigits 2 <?> "second"
  guard (sec <= 60) <?> "second between 0 and 60"
  millis <- join ((nDigits 3 <$ Atto.char '.') <|> pure (pure 0)) <?> "millisecond"
  guard (millis < 1000) <?> "millis between 0 and 999"
  return $ picosecondsToDiffTime $ fromIntegral
    $ (* 1e9) $ (+ millis) -- psecs
    $ (* 1e3) $ (+ sec)    -- msecs
    $ (* 60)  $ (+ mins)   -- secs
    $ (* 60)  $ hours      -- mins

toDateOnly :: Atto.Parser Day
toDateOnly = (<?> "date") $ delimited parseDate

toTimeOnly :: Atto.Parser DiffTime
toTimeOnly = (<?> "time of day") $ delimited parseTimeOfDay

toTimestamp :: Atto.Parser UTCTime
toTimestamp = (<?> "timestamp") $ delimited $
  UTCTime <$> (parseDate <* Atto.char '-') <*> parseTimeOfDay

toMonthYear :: Atto.Parser Day
toMonthYear = (<?> "month-year") $ delimited $ do
  year <- fromIntegral <$> nDigits 4
  mon  <- nDigits 2
  maybe (fail "invalid date") return $ fromGregorianValid year mon 1

skipToken :: Atto.Parser ()
skipToken = Atto.skipWhile (FIX.delimiter /=)

nDigits :: Int -> Atto.Parser Int
nDigits n = (<?> show n ++ " digits") $ do
  ds <- replicateM n Atto.digit
  return $ foldl' (\r c -> r * 10 + c2d c) 0 ds

c2d :: Char -> Int
c2d c = ord c .&. 0x0f

w2d :: Word8 -> Int
w2d w = fromIntegral w .&. 0x0f
