{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Test.Common where

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as C

maxShowLen :: Int
maxShowLen = 80

showLong :: Show a => a -> String
showLong bs
    | length ss > maxShowLen = take (maxShowLen - 4) ss <> "...\""
    | otherwise = ss
    where
      ss = show bs

runParser :: Atto.Parser a -> C.ByteString -> Either String a
runParser p = Atto.parseOnly (p <* Atto.endOfInput)

soh :: C.ByteString -> C.ByteString
soh = (<> "\SOH")
