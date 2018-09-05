{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FIX.Split
  ( splitting, Error(..)
  ) where

import Data.Char (ord)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy

import Data.FIX.Spec (FIXVersion(..), fixSpec)
import Data.FIX.Message (FIXSpec(fsVersion))

fixPrefix :: FIXVersion -> BS.Lazy.ByteString
fixPrefix v = BS.Lazy.fromStrict ("8=" <> str <> "\SOH9=")
  where
    str = BS.pack (map (fromIntegral . ord) (fsVersion (fixSpec v)))

data Error
  = PartialParse BS.Lazy.ByteString  -- ^ Expected more data
  | BadParse BS.Lazy.ByteString      -- ^ Message didn't parse correctly
  | BadChecksum !Word8 !Word8        -- ^ Checksum didn't match, (@expected@, @parsed@)
    deriving (Eq, Ord, Show)

splitting
  :: Monad m
  => FIXVersion
  -> m BS.ByteString               -- ^ Read the next block of bytes (strict @ByteString@)
  -> (Error -> m ())               -- ^ Report an error
  -> (BS.Lazy.ByteString -> m ())  -- ^ Do something with a parsed message (lazy @ByteString@)
  -> m ()
splitting fixVersion getBS failed yield = loopHead ""
  where
    loopHead bs
      | BS.Lazy.length bs < prefixLen ||
        isNothing (BS.Lazy.elemIndex 0x01 (BS.Lazy.drop prefixLen bs))
      = do buf <- getBS
           case (BS.null buf, BS.Lazy.null bs) of
             (False,     _) -> loopHead (bs <> BS.Lazy.fromStrict buf)
             ( True,  True) -> return ()
             ( True, False) -> failed (PartialParse bs)
      | otherwise
      = parseHeading prefix bs failed $ \n ->
          loopBody (n + prefixLen) bs
    loopBody i bs
      | BS.Lazy.length bs < i + 7  -- 7 comes from "10=xxx\SOH" for the checksum
      = do buf <- getBS
           if BS.null buf
             then failed (PartialParse bs)
             else loopBody i (bs <> BS.Lazy.fromStrict buf)
      | otherwise
      = do let (msg, bs') = BS.Lazy.splitAt i bs
           parseChecksum msg bs' failed $ \_ chksum bs'' ->
             yield (msg <> chksum) >> loopHead bs''
    prefix = fixPrefix fixVersion
    prefixLen = BS.Lazy.length prefix

parseHeading
  :: BS.Lazy.ByteString -- ^ Prefix bytes
  -> BS.Lazy.ByteString -- ^ Bytes to parse for header
  -> (Error -> r)       -- ^ On error
  -> (Int64 -> r)       -- ^ Success: body length in bytes
  -> r
parseHeading prefix bs failed ok
  | BS.Lazy.isPrefixOf prefix bs = ok (1 + n + BS.Lazy.length ns)
  | otherwise = failed (BadParse bs)
  where
    ns = BS.Lazy.takeWhile (/= 0x01) (BS.Lazy.drop (BS.Lazy.length prefix) bs)
    n  = BS.Lazy.foldl' (\acc x -> (acc * 10) + fromIntegral (x - 48)) (0 :: Int64) ns
{-# INLINE parseHeading #-}

parseChecksum
  :: BS.Lazy.ByteString                 -- ^ Message to checksum
  -> BS.Lazy.ByteString                 -- ^ Bytes to parse for checksum
  -> (Error -> r)                       -- ^ On error
  -> (Word8 -> BS.Lazy.ByteString -> BS.Lazy.ByteString -> r)
     -- ^ Success: checksum, checksum bytes, and beginning of next message
  -> r
parseChecksum msg bs failed ok =
  case (prefix, parsed == expected) of
    ("10=",  True) -> ok parsed (BS.Lazy.take 7 bs) (BS.Lazy.drop 1 bs')
    ("10=", False) -> failed (BadChecksum expected parsed)
    (    _,     _) -> failed (BadParse bs)
  where
    expected = BS.Lazy.foldl' (+) 0 msg
    (prefix, (ns, bs')) = BS.Lazy.splitAt 3 <$> BS.Lazy.splitAt 3 bs
    parsed = BS.Lazy.foldl' (\acc x -> (acc * 10) + fromIntegral (x - 48)) (0 :: Word8) ns
{-# INLINE parseChecksum #-}
