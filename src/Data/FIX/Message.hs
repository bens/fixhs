-- mODULE  : Data.FIX.Message
-- License : LGPL-2.1

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | FIX messages
module Data.FIX.Message
    ( FIXValue (..)
    , FIXTag (..)
    , FIXMessage (..)
    , FIXGroupSpec (..)
    , FIXGroupElement (..)
    , FIXSpec (..)
    , FIXMessageSpec (..)
    , FIXMessageSpecs
    , checksum
    , delimiter
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..), DiffTime)
import Test.QuickCheck (Gen)

import Data.Coparser (BuilderLike (..), foldl')
import Data.FIX.Common (Parser, delimiter)

-- | A valid FIX field description. It is used to specify FIX messages using
-- 'FIXMessageSpec'.
data FIXTag = FIXTag
    { tName :: String
    -- ^ The name of the tag e.g. BeginString.
    , tnum :: Int
    -- ^ The numerical value of the tag e.g. 8.
    , tparser :: Parser FIXValue
    -- ^ The corresponding attoparsec parser.
    , genValue :: Gen FIXValue
    -- ^ A random generator for that particular types of fields.
    }
instance Eq FIXTag where
    x == y = tnum  x == tnum  y

instance Show FIXTag where
    show tag = "FIXTag " ++ show (tnum tag)

data FIXGroupElement = FIXGroupElement Int FIXValue (IntMap FIXValue)
    deriving (Eq, Show)
instance NFData FIXGroupElement where
    rnf (FIXGroupElement n s vs) = rnf n `seq` rnf s `seq` rnf vs

data FIXValue
    = FIXInt Int
    | FIXDouble Double
    | FIXChar Char
    | FIXBool Bool
    | FIXString ByteString
    | FIXData ByteString
      -- FIXME: seems better to contain a list of bytestrings
    | FIXMultipleValueString ByteString
    | FIXTimestamp UTCTime
    | FIXTimeOnly DiffTime
    | FIXDateOnly Day
    | FIXMonthYear Day  -- FIXME: should this be a pair of (Integer, Int)?
    | FIXGroup Int [FIXGroupElement]
    deriving (Eq, Show)
instance NFData FIXValue where
    rnf (FIXInt x) = rnf x
    rnf (FIXDouble x) = rnf x
    rnf (FIXChar x) = rnf x
    rnf (FIXBool x) = rnf x
    rnf (FIXString x) = rnf x
    rnf (FIXMultipleValueString x) = rnf x
    rnf (FIXTimestamp x) = rnf x
    rnf (FIXTimeOnly x) = rnf x
    rnf (FIXDateOnly x) = rnf x
    rnf (FIXMonthYear x) = rnf x
    rnf (FIXData x) = rnf x
    rnf (FIXGroup l es) = rnf l `seq` rnf es

data FIXMessage = FIXMessage
    { mContext :: FIXSpec
    , mType    :: ByteString
    , mHeader  :: IntMap FIXValue
    , mBody    :: IntMap FIXValue
    , mTrailer :: IntMap FIXValue
    }
instance Eq FIXMessage where
  x == y = and
    [ mType x    == mType y
    , mHeader x  == mHeader y
    , mBody x    == mBody y
    , mTrailer x == mTrailer y
    ]
instance Show FIXMessage where
    showsPrec d (FIXMessage spec ty hd bd tr) =
        showParen (d > app_prec) $
            showString "FIXMessage "
            . showString (fsVersion spec) . showString " "
            . showsPrec (app_prec+1) ty . showString " "
            . showsPrec (app_prec+1) hd . showString " "
            . showsPrec (app_prec+1) bd . showString " "
            . showsPrec (app_prec+1) tr
      where app_prec = 10

instance NFData FIXMessage where
    rnf (FIXMessage _ _ h b t) = rnf h `seq` rnf b `seq` rnf t

data FIXMessageSpec = FMSpec
    { msName    :: String
    , msType    :: ByteString
    , msHeader  :: IntMap FIXTag
    , msBody    :: IntMap FIXTag
    , msTrailer :: IntMap FIXTag
    } deriving (Eq, Show)

type FIXMessageSpecs
    = Map ByteString FIXMessageSpec

data FIXSpec = FSpec
    { fsVersion  :: String          -- ^ FIX version
    , fsHeader   :: IntMap FIXTag   -- ^ FIX header tags
    , fsTrailer  :: IntMap FIXTag   -- ^ FIX trailer tags
    , fsMessages :: FIXMessageSpecs -- ^ Dictionary of all FIX messages
    , fsTags     :: IntMap FIXTag   -- ^ Dictionary of all FIX tags
    } deriving (Eq, Show)

data FIXGroupSpec = FGSpec
    { gsLength    :: FIXTag
    , gsSeparator :: FIXTag
    , gsBody      :: IntMap FIXTag
    }

-- FIX checksum is simply the sum of bytes modulo 256
checksum :: BuilderLike t => t -> (t, Int)
checksum b = (b, foldl' (\t c -> t + ord c) 0 b `mod` 256)
