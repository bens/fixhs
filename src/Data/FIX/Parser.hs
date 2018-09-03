-- Module  : Data.FIX.Parser
-- License : LGPL-2.1

{-# OPTIONS_GHC  -fno-warn-missing-signatures #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.FIX.Parser
    (
-- * Introduction
-- | In order to get a Parser 'FIXMessage' 'FIXSpec' you can
--
-- @
--import qualified Data.FIX.Parser as FIX ( nextP, messageP )
--
--FIX.nextP >>= FIX.messageP
-- @
      messageP
    , groupP
    -- , nextP
    -- , nextP'
    , toFIXInt
    , toFIXChar
    , toFIXString
    , toFIXDouble
    , toFIXBool
    , toFIXMultipleValueString
    , toFIXTimestamp
    , toFIXTimeOnly
    , toFIXData
    , toFIXDateOnly
    , toFIXMonthYear
    , tBeginString
    , tCheckSum
    , tBodyLength
    , tMsgType

    -- * Internal
    , tagP
    , tagsP
    ) where

import Control.Applicative ((<$>), many)
import Control.Monad (replicateM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Test.QuickCheck (arbitrary)
import Text.Printf (printf)
import Text.Megaparsec ((<?>), runParser, takeP, try)

import qualified Data.ByteString.Char8 as C
import qualified Data.FIX.Message as FIX
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Text.Megaparsec.Byte as Mega

import Data.FIX.Arbitrary (genByteString)
import Data.FIX.Common (Parser)
import Data.FIX.Message

import qualified Data.FIX.ParserCombinators as FIX


-- parse a specific tag and its value
tagP :: FIXTag -> Parser FIXValue
tagP tag = do
    t <- FIX.toTag <?> "tag #" ++ show (tnum tag)
    -- if the two tags coincide read the value
    when (t /= tnum tag) $
        fail "wrong tag"
    tparser tag

-- parse all the specificed tags and their corresponding values
tagsP :: IntMap FIXTag -> Parser (IntMap FIXValue, IntMap FIXTag)
tagsP = evalStateT $ do
    vals <- many parseTag
    unparsed <- get
    return (mconcat vals, unparsed)
    where
        parseTag :: StateT (IntMap FIXTag) Parser (IntMap FIXValue)
        parseTag = try $ do
            t <- lift FIX.toTag
            ts <- get
            modify (IntMap.delete t)
            case IntMap.lookup t ts of
                Nothing ->
                    fail ("unexpected tag: " ++ show t)
                Just tag -> do
                    v <- lift $ tparser tag
                    return (IntMap.singleton t v)

-- parse a value of type FIX group
groupP :: FIXGroupSpec -> Parser FIXValue
groupP spec = do
    nTag <- tparser lenTag <?> "group length"
    case nTag of
        FIXInt n -> FIXGroup n <$> replicateM n submsg
        _ -> fail ("failed to parse group: " ++ show (tnum lenTag))
    where
        submsg :: Parser FIXGroupElement
        submsg = do
            sep <- tagP sepTag             -- The separator of the message
            (vs, _) <- tagsP (gsBody spec) -- The rest of the message
            return (FIXGroupElement (tnum sepTag) sep vs)
        lenTag = gsLength spec
        sepTag = gsSeparator spec

-- | Match the next FIX message (only text) in the stream. The checksum is
-- validated.
snarfMessageP :: Parser ByteString
snarfMessageP = do
    (hdrChksum, len) <- headerP <?> "header"
    msg <- takeP Nothing len <?> "snarfing body"
    let chksum = (hdrChksum + snd (FIX.checksum msg)) `mod` 256
    -- after snarfing the message body, parse the checksum given
    c <- checksumP <?> "checksum"
    if chksum == c
        then return msg
        else fail $ printf "checksum is not valid: calculated as %d, expecting %d" chksum c
    where
        -- Parse header and return checksum and length.
        -- A header always starts with the version tag (8)
        -- followed by the length tag (9). Note: these 2 tags
        -- are included in the checksum
        headerP :: Parser (Int, Int)
        headerP = do
            let p8 = do
                    (_, c)  <- FIX.checksum <$> Mega.string (C.pack "8=")
                    (_, c') <- FIX.checksum <$> FIX.toString
                    return (c + c')
            let p9 = do
                    (_, c)  <- FIX.checksum <$> Mega.string (C.pack "9=")
                    (l, c') <- FIX.checksum <$> FIX.toString
                    return (c + c', l)
            c8      <- p8 <?> "8 (BeginString)"
            (c9, l) <- p9 <?> "9 (BodyLength)"
            let c = (c8 + c9 + 2) `mod` 256
            return (c, FIX.toInt' l)
        checksumP :: Parser Int
        checksumP = Mega.string (C.pack "10=") >> FIX.toInt

messageP :: FIXSpec -> Parser FIXMessage
messageP spec = do
    msg <- snarfMessageP <?> "snarfing message"
    either (fail . show) return $ runParser psr "" msg
  where
    psr = do
      FIXString msgType <- tagP tMsgType <?> "message type"
      case Map.lookup msgType (fsMessages spec) of
          Nothing ->
              fail ("unknown message type: " ++ show msgType)
          Just msgSpec -> do
              hdr <- fst <$> tagsP (msHeader  msgSpec) <?> "header"
              bdy <- fst <$> tagsP (msBody    msgSpec) <?> "body"
              trl <- fst <$> tagsP (msTrailer msgSpec) <?> "trailer"
              return (FIXMessage spec msgType hdr bdy trl)

-- FIX value parsers
toFIXInt                 = FIXInt                 <$> FIX.toInt
toFIXDouble              = FIXDouble              <$> FIX.toDouble
toFIXBool                = FIXBool                <$> FIX.toBool
toFIXString              = FIXString              <$> FIX.toString
toFIXMultipleValueString = FIXMultipleValueString <$> FIX.toString
toFIXData                = FIXData                <$> FIX.toString
toFIXTimestamp           = FIXTimestamp           <$> FIX.toTimestamp
toFIXTimeOnly            = FIXTimeOnly            <$> FIX.toTimeOnly
toFIXChar                = FIXChar                <$> FIX.toChar
toFIXDateOnly            = FIXDateOnly            <$> FIX.toDateOnly
toFIXMonthYear           = FIXMonthYear           <$> FIX.toMonthYear


tBeginString :: FIXTag
tBeginString = FIXTag
    { tName = "BeginString"
    , tnum = 8
    , tparser = toFIXString
    , genValue = FIXString <$> genByteString }


tBodyLength :: FIXTag
tBodyLength = FIXTag
    { tName = "BodyLength"
    , tnum = 9
    , tparser = toFIXInt
    , genValue = FIXInt <$> arbitrary }

tMsgType :: FIXTag
tMsgType = FIXTag
    { tName = "MsgType"
    , tnum = 35
    , tparser = toFIXString
    , genValue = FIXString <$> genByteString }

tCheckSum :: FIXTag
tCheckSum = FIXTag
    { tName = "CheckSum"
    , tnum = 10
    , tparser = toFIXInt
    , genValue = FIXInt <$> arbitrary }
