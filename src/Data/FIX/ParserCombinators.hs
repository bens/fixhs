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
    ) where

import Prelude hiding (null, tail, head)

import Control.Applicative ((<$>), (<|>))
import Control.Applicative.Combinators (skipMany)
import Control.Monad (guard, join, replicateM)
import Data.Bits ((.&.))
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.List (foldl')
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (DiffTime, UTCTime (..), picosecondsToDiffTime)
import Text.Megaparsec ((<?>), takeWhile1P)

import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Text.Megaparsec.Byte as Mega
import qualified Text.Megaparsec.Byte.Lexer as Mega

import Data.FIX.Common (Parser)

pChar :: Char -> Parser Char
pChar c = c <$ Mega.char (fromIntegral (ord c))

delimited :: Parser a -> Parser a
delimited m = m <* Mega.char 0x01

toDouble :: Parser Double
toDouble = (<?> "float value") $ delimited $ Mega.signed (pure ()) $ do
    a <- Mega.decimal :: Parser Integer
    (!m, !e) <- join (((extract_decimals <$> snarfTillDelim) <$ pChar '.') <|> pure (pure (0, 1)))
    return $! fromIntegral a + fromIntegral m / (10^e)
    where
        extract_decimals :: BS.Lazy.ByteString -> (Int, Int)
        extract_decimals = BS.Lazy.foldl' helper (0, 0)
            where
                helper (!m, !e) c = (m * 10 + w2d c, succ e)

snarfTillDelim :: Parser BS.Lazy.ByteString
snarfTillDelim = takeWhile1P Nothing (/= 0x01)

parseIntTill :: Char -> Parser Int
parseIntTill c = Mega.signed (pure ()) Mega.decimal <* Mega.char (fromIntegral (ord c))

toInt' :: BS.Lazy.ByteString -> Int
toInt' = BS.Lazy.foldl' (\r c -> r * 10 + w2d c) 0

toInt :: Parser Int
toInt = delimited (Mega.signed (pure ()) Mega.decimal) <?> "int value"

toChar :: Parser Char
toChar = chr . fromIntegral <$> (delimited Mega.anyChar <?> "char value")

toString :: Parser BS.Lazy.ByteString
toString = delimited snarfTillDelim <?> "string value"

toTag :: Parser Int
toTag = parseIntTill '=' <?> "FIX tag"

toBool :: Parser Bool
toBool = (<?> "boolean") $ delimited (f <$> p)
    where
        p = pChar 'Y' <|> pChar 'N'
        f 'Y' = True
        f 'N' = False
        f _   = error "wrong boolean FIX value"

parseDate :: Parser Day
parseDate = do
  year <- (fromIntegral <$> nDigits 4) <?> "year"
  mon  <- nDigits 2 <?> "month"
  day  <- nDigits 2 <?> "day"
  maybe (fail "invalid date") return $ fromGregorianValid year mon day

parseTimeOfDay :: Parser DiffTime
parseTimeOfDay = do
  hours  <- nDigits 2 <* pChar ':' <?> "hour"
  guard (hours < 24) <?> "hour between 0 and 23"
  mins   <- nDigits 2 <* pChar ':' <?> "minute"
  guard (mins < 60) <?> "minute between 0 and 59"
  sec    <- nDigits 2 <?> "second"
  guard (sec <= 60) <?> "second between 0 and 60"
  millis <- join ((nDigits 3 <$ pChar '.') <|> pure (pure 0)) <?> "millisecond"
  guard (millis < 1000) <?> "millis between 0 and 999"
  return $ picosecondsToDiffTime $ fromIntegral
    $ (* 1e9) $ (+ millis) -- psecs
    $ (* 1e3) $ (+ sec)    -- msecs
    $ (* 60)  $ (+ mins)   -- secs
    $ (* 60)  $ hours      -- mins

toDateOnly :: Parser Day
toDateOnly = (<?> "date") $ delimited parseDate

toTimeOnly :: Parser DiffTime
toTimeOnly = (<?> "time of day") $ delimited parseTimeOfDay

toTimestamp :: Parser UTCTime
toTimestamp = (<?> "timestamp") $ delimited $
  UTCTime <$> (parseDate <* pChar '-') <*> parseTimeOfDay

toMonthYear :: Parser Day
toMonthYear = (<?> "month-year") $ delimited $ do
  year <- fromIntegral <$> nDigits 4
  mon  <- nDigits 2
  maybe (fail "invalid date") return $ fromGregorianValid year mon 1

skipToken :: Parser ()
skipToken = skipMany (Mega.notChar 0x01)

nDigits :: Int -> Parser Int
nDigits n = (<?> show n ++ " digits") $ do
  ds <- replicateM n (chr . fromIntegral <$> Mega.digitChar)
  return $ foldl' (\r c -> r * 10 + c2d c) 0 ds

c2d :: Char -> Int
c2d c = ord c .&. 0x0f

w2d :: Word8 -> Int
w2d w = fromIntegral w .&. 0x0f
