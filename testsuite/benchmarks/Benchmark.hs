{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ( foldr )
import qualified Prelude as P ( foldr )
import Gauge.Main (nf)
import qualified Gauge.Main as Gauge
import qualified Gauge.Main.Options as Gauge
import Data.FIX.Spec.FIX42
import Data.FIX.Parser
import Data.FIX.Coparser
import Data.FIX.Message
import Data.Attoparsec.ByteString hiding (take, parse)
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT
import Test.QuickCheck.Gen
import Control.Applicative ( (<$>) )
import Data.Coparser ( unpack )
import Data.FIX.Arbitrary

myConfig :: Gauge.Config
myConfig = Gauge.defaultConfig

samples :: FIXSpec -> IO [FIXMessage FIXSpec]
samples spec =
    let ms = map snd $ LT.toList $ fsMessages spec in
        mapM genSample ms
    where
       genSample :: FIXMessageSpec -> IO (FIXMessage FIXSpec)
       genSample m' = let randMsg = arbitraryFIXMessage spec in
                          head <$> sample' (randMsg m')

benchmark :: FIXSpec -> [FIXMessage FIXSpec] -> [Gauge.Benchmark]
benchmark spec ss =
    let ms = map snd $ LT.toList $ fsMessages spec
        parsingB (m, input) = let input' = coparse input :: ByteString in
            Gauge.bench (msName m ++ " parsing") $
                nf (parse (nextP >>= messageP fix42)) (C.pack $ unpack input')
        coparsingB (m, input) = Gauge.bench (msName m ++ " coparsing") $
                nf (coparse :: FIXMessage FIXSpec -> ByteString ) input

        bench1 = zipWith (curry coparsingB) ms ss
        bench2 = zipWith (curry parsingB) ms ss
    in
        ziczac bench1 bench2

    where
        parse :: Parser (FIXMessage FIXSpec) -> ByteString -> FIXMessage FIXSpec
        parse p b = case parseOnly p b of
                Left err -> error err
                Right m -> m
        ziczac :: [a] -> [a] -> [a]
        ziczac l r = P.foldr _construct [] $ zip l r
            where
                _construct :: (a, a) -> [a] -> [a]
                _construct (l', r') as = l' : r' : as

main :: IO ()
main = do
        ss <- samples fix42
        Gauge.defaultMainWith myConfig [ Gauge.bgroup "FIX42" $ benchmark fix42 ss ]
