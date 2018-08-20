-- Module  : Data.FIX.ParserCombinators
-- License : LGPL-2.1

{-# LANGUAGE BangPatterns #-}

module Data.FIX.ParserCombinators
    ( toTag
    , toString
    , toInt'
    , toInt
    , toDouble
    , toBool
    , toTimestamp
    , toTimeOnly
    , toChar
    , toDateOnly
    , toMonthYear

    , skipToken

    -- exporting Attoparsec
    , Data.Attoparsec.ByteString.Parser
    , Data.Attoparsec.Zepto.parse
    ) where

import Prelude hiding ( null, tail, head )
import Data.Attoparsec.ByteString ( Parser )
import qualified Data.Attoparsec.Zepto ( parse )
import Data.Attoparsec.ByteString.Char8
    ( skipWhile, signed, char, char8, anyChar, takeWhile1, decimal )
import Data.Char ( ord )
import Data.ByteString hiding ( pack, putStrLn )
import Control.Applicative ( (<$>), (<|>), (*>) )
import Control.Monad (void)
import Data.Time.Calendar ( Day, fromGregorian )
import Data.Time.Clock ( DiffTime, UTCTime (..), picosecondsToDiffTime )
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.FIX.Common as FIX ( delimiter )


skipFIXDelimiter :: Parser ()
skipFIXDelimiter = void (char8 FIX.delimiter)


toDouble :: Parser Double
toDouble =  signed $ do
    a <- decimal :: Parser Integer
    (!m, !e) <- (char '.' *> (extract_decimals <$> toString)) <|> (skipFIXDelimiter >> return (0, 1))
    return $ fromIntegral a + fromIntegral m / fromIntegral e
    where
        extract_decimals :: ByteString -> (Int, Int)
        extract_decimals = foldl' helper (0, 1)
                where
                        helper (!m, !e) c = (m * 10 + fromIntegral c - ord '0', 10 * e)

parseIntTill :: Char -> Parser Int
parseIntTill c = do
    i <- signed decimal
    _ <- char8 c
    return i

toInt' :: ByteString -> Int
toInt' = helper 0
           where
                helper i j
                    | null j    = i
                    | otherwise =
                        helper (10 * i + fromIntegral (head j) - ord '0') (tail j)

toInt :: Parser Int
toInt = parseIntTill FIX.delimiter


toChar :: Parser Char
toChar = do
    c <- anyChar
    skipFIXDelimiter
    return c

toString :: Parser ByteString
toString = do
    str <- takeWhile1 (/= FIX.delimiter)
    skipFIXDelimiter
    return str


toTag :: Parser Int
toTag = parseIntTill '='

toBool :: Parser Bool
toBool = do
    c <- char 'Y' <|> char 'N'
    skipFIXDelimiter
    case c of
        'Y' -> return True
        'N' -> return False
        _ -> error "wrong boolean FIX value"

toSecMillis :: Parser (Int, Int)
toSecMillis = do
   (sec, mil) <- read_sec_millis <|> (toInt >>= (\s -> return (s, 0)))
   return (sec, mil)
   where
        read_sec_millis :: Parser (Int, Int)
        read_sec_millis = do
            sec' <- parseIntTill '.'
            mil' <- toInt
            return (sec', mil')

toDay :: Int -> Day
toDay i = fromGregorian year month day
  where
    year  = fromIntegral (i `div` 10000)
    rest  = i `mod` 10000
    month = rest `div` 100
    day   = rest `mod` 100

toTimestamp :: Parser UTCTime
toTimestamp = do
   doy <- toDay <$> parseIntTill '-'
   UTCTime doy <$> toTimeOnly

toTimeOnly :: Parser DiffTime
toTimeOnly = do
   hours   <- parseIntTill ':'
   minutes <- parseIntTill ':'
   (sec, milli) <- toSecMillis
   return $ picosecondsToDiffTime $ fromIntegral
     $ (* 1000000)            -- picosecs
     $ (* 1000) $ (+ milli)   -- usecs
     $ (* 1000) $ (+ sec)     -- msecs
     $ (* 60)   $ (+ minutes)
     $ (* 60)   $ hours

toDateOnly :: Parser Day
toDateOnly = toDay <$> toInt

toMonthYear :: Parser Day
toMonthYear = do
   i <- toInt
   let year  = fromIntegral (i `div` 100)
   let month = i `mod` 100
   return (fromGregorian year month 1)

skipToken :: Parser ()
skipToken = skipWhile (FIX.delimiter /=)
