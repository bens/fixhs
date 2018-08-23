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
import Data.ByteString (ByteString)
import Data.Monoid (mappend, mconcat)
import GHC.Float (showFloat)
import Data.String (IsString(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BB
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

instance BuilderLike ByteString where
    unpack = C.unpack
    singleton = C.singleton
    append = B.append
    cons = C.cons
    snoc = C.snoc
    concat = B.concat
    length = B.length
    foldl' f = let f' !x !w = {-# SCC "Urvli" #-} w82c w `seq` x `seq` f x (w82c w) in B.foldl' f'
    foldl f = let  f' x =  f x . w82c in B.foldl f'

instance BuilderLike (DL.DList Char) where
    unpack = DL.toList
    singleton = DL.singleton
    append = DL.append
    cons = DL.cons
    snoc = DL.snoc
    concat = DL.concat

instance BuilderLike BB.Builder where
    pack = BB.stringUtf8
    unpack = BL.unpack . BB.toLazyByteString
    singleton = BB.byteString . C.singleton
    append = mappend
    concat = mconcat
    cons c b = BB.charUtf8 c <> b

instance BuilderLike LazyTextB.Builder where
    unpack = LazyText.unpack . LazyTextB.toLazyText
    singleton = LazyTextB.singleton
    append = mappend
    concat = mconcat

class Coparser a where
    coparse :: BuilderLike t => a -> t
