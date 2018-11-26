{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Split
  ( splitting, Error(..)
  ) where

import Data.Char (ord)
import Data.Word (Word8)

import qualified Data.Attoparsec.ByteString       as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto.C8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BS.Lazy

import Data.FIX.Message (FIXSpec(fsVersion))
import Data.FIX.Spec    (FIXVersion, fixSpec)

fixPrefix :: FIXVersion -> BS.Lazy.ByteString
fixPrefix v = BS.Lazy.fromStrict ("8=" <> str <> "\SOH9=")
  where
    str = BS.pack (map (fromIntegral . ord) (fsVersion (fixSpec v)))

data Error
  = PartialParse                     -- ^ Expected more data
  | BadParse String                  -- ^ Message didn't parse correctly
  | BadChecksum !Word8 !Word8        -- ^ Checksum didn't match, (@expected@, @parsed@)
    deriving (Eq, Ord, Show)

pSplit :: FIXVersion -> Atto.Parser (Either (Word8, Word8) BS.ByteString)
pSplit fixVersion = do
  _       <- Atto.string (BS.Lazy.toStrict $ fixPrefix fixVersion)
  n       <- Atto.C8.decimal <* Atto.word8 0x01
  bs      <- Atto.take n
  [a,b,c] <- Atto.string "10=" *> (BS.unpack <$> Atto.take 3) <* Atto.word8 0x01
  let expected = (100 * (a-48)) + (10 * (b-48)) + (c-48)
      parsed   = BS.foldl' (+) 0 bs
  if parsed == expected then pure (Right bs) else pure (Left (expected, parsed))

splitting
  :: Monad m
  => FIXVersion
  -> m BS.ByteString
     -- ^ Read the next block of bytes (strict @ByteString@)
  -> (Error -> m ())
     -- ^ Report an error
  -> (BS.ByteString -> m ())
     -- ^ Do something with a parsed message (lazy @ByteString@)
  -> m ()
splitting fixVersion getBS failed yield = loop ""
  where
    loop bs = Atto.parseWith getBS (pSplit fixVersion) bs >>= \case
      Atto.Fail _remaining _ctxs msg -> failed (BadParse msg)
      Atto.Partial _ -> failed PartialParse
      Atto.Done remaining (Right x) -> yield x >> loop remaining
      Atto.Done remaining (Left (expected, parsed)) ->
        failed (BadChecksum expected parsed) >> loop remaining
