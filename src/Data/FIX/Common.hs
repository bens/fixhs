-- Module  : Data.FIX.Common
-- License : LGPL-2.1

module Data.FIX.Common
    ( Parser, delimiter
    ) where

import Data.ByteString (ByteString)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void ByteString

delimiter :: Char
delimiter = '\SOH'
