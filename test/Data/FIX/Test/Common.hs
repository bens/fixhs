{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Test.Common where

import Data.ByteString (ByteString)

import qualified Text.Megaparsec as Mega

import Data.FIX.Common (Parser)

maxShowLen :: Int
maxShowLen = 80

showLong :: Show a => a -> String
showLong bs
    | length ss > maxShowLen = take (maxShowLen - 4) ss <> "...\""
    | otherwise = ss
    where
      ss = show bs

runParser :: Parser a -> ByteString -> Either String a
runParser p bs =
  either (Left . Mega.parseErrorPretty' bs) Right
  $ Mega.runParser (p <* Mega.eof) "" bs

soh :: ByteString -> ByteString
soh = (<> "\SOH")
