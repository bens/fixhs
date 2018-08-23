{-# LANGUAGE ScopedTypeVariables #-}

import Data.Attoparsec.ByteString hiding (take, parse)
import Data.Coparser (unpack)
import Data.FIX.Arbitrary
import Data.FIX.Coparser
import Data.FIX.Message
import Data.FIX.Parser
import Data.FIX.Spec.FIX42
import Gauge.Main (nf)
import Test.QuickCheck.Gen

import qualified Data.ByteString.Char8 as BSC
import qualified Data.LookupTable as LT
import qualified Gauge.Main as Gauge
import qualified Gauge.Main.Options as Gauge

myConfig :: Gauge.Config
myConfig = Gauge.defaultConfig

samples :: FIXSpec -> IO [FIXMessage]
samples spec =
    let ms = map snd $ LT.toList $ fsMessages spec in
        mapM genSample ms
    where
       genSample :: FIXMessageSpec -> IO FIXMessage
       genSample = fmap head . sample' . genFIXMessage' spec

benchmark :: FIXSpec -> [FIXMessage] -> [Gauge.Benchmark]
benchmark spec ss =
    let ms = map snd $ LT.toList $ fsMessages spec
        parsingB (m, input) = let input' = coparse input :: BSC.ByteString in
            Gauge.bench (msName m ++ " parsing bytestring") $
                nf (parse (messageP fix42)) (BSC.pack $ unpack input')
        coparsingB (m, input) =
            Gauge.bench (msName m ++ " coparsing bytestring") $
                nf (coparse :: FIXMessage -> BSC.ByteString) input

        bench1 = zipWith (curry coparsingB) ms ss
        bench2 = zipWith (curry parsingB) ms ss
    in
        ziczac bench1 bench2

    where
        parse :: Parser FIXMessage -> BSC.ByteString -> FIXMessage
        parse p b = either error id (parseOnly p b)
        ziczac :: [a] -> [a] -> [a]
        ziczac l r = foldMap f (zip l r) where f (l', r') = [l', r']

main :: IO ()
main = do
        ss <- samples fix42
        Gauge.defaultMainWith myConfig
            [ Gauge.bgroup "FIX42" $ benchmark fix42 ss
            ]
