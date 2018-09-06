-- Module  : Data.Coparser
-- License : LGPL-2.1

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Coparser
    ( Coparser (..)
    , BuilderLike (..)
    ) where

import Data.Bits.Utils (w82c)
import Data.Monoid (mappend, mconcat)
import GHC.Float (showFloat)
import Data.String (IsString(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.C8
import qualified Data.ByteString.Builder as BS.B
import qualified Data.DList as DL
import qualified Data.Foldable as P
import qualified Data.List as L
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyTextB

class (Monoid cs, IsString cs) => BuilderLike cs where
    pack :: String -> cs
    pack = fromString
    unpack :: cs -> String
    singleton :: Char -> cs
    append :: cs -> cs -> cs
    concat :: [cs] -> cs
    cons :: Char -> cs -> cs
    snoc :: cs -> Char -> cs
    decimal :: Integral i => i -> cs
    realFloat :: RealFloat r => r -> cs

    decimal = pack . show . toInteger
    realFloat r = pack $ showFloat r ""
    cons c t = singleton c `append` t
    snoc t c = t `append` singleton c

    length :: cs -> Int
    length = P.length . unpack

    foldl' :: (b -> Char -> b) -> b -> cs -> b
    foldl' f x0 = P.foldl' f x0 . unpack

    foldl :: (b -> Char -> b) -> b -> cs -> b
    foldl f x0 = P.foldl f x0 . unpack

instance BuilderLike String where
    unpack = id
    singleton c = [c]
    append = (++)
    cons = (:)
    concat = L.concat
    length = P.length
    foldl' = P.foldl'
    foldl = P.foldl

instance BuilderLike BS.ByteString where
    unpack = BS.C8.unpack
    singleton = BS.C8.singleton
    append = BS.append
    cons = BS.C8.cons
    snoc = BS.C8.snoc
    concat = BS.concat
    length = BS.length
    foldl' f = let f' !x !w = {-# SCC "Urvli" #-} w82c w `seq` x `seq` f x (w82c w) in BS.foldl' f'
    foldl f = let  f' x =  f x . w82c in BS.foldl f'

instance BuilderLike BS.Lazy.ByteString where
    unpack = BS.Lazy.C8.unpack
    singleton = BS.Lazy.C8.singleton
    append = BS.Lazy.append
    cons = BS.Lazy.C8.cons
    snoc = BS.Lazy.C8.snoc
    concat = BS.Lazy.concat
    length = fromIntegral . BS.Lazy.length
    foldl' f = let f' !x !w = {-# SCC "Urvli" #-} w82c w `seq` x `seq` f x (w82c w) in BS.Lazy.foldl' f'
    foldl f = let  f' x =  f x . w82c in BS.Lazy.foldl f'

instance BuilderLike (DL.DList Char) where
    unpack = DL.toList
    singleton = DL.singleton
    append = DL.append
    cons = DL.cons
    snoc = DL.snoc
    concat = DL.concat

instance BuilderLike BS.B.Builder where
    pack = BS.B.stringUtf8
    unpack = BS.Lazy.C8.unpack . BS.B.toLazyByteString
    singleton = BS.B.byteString . BS.C8.singleton
    append = mappend
    concat = mconcat
    cons c b = BS.B.charUtf8 c <> b

instance BuilderLike LazyTextB.Builder where
    unpack = LazyText.unpack . LazyTextB.toLazyText
    singleton = LazyTextB.singleton
    append = mappend
    concat = mconcat

class Coparser a where
    coparse :: BuilderLike t => a -> t
