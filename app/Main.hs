{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Category ((<<<))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Internal as BSB
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as BS
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (Down))
import Data.Word (Word8)
import System.IO (Handle, IOMode (WriteMode), hFlush, stdout, withBinaryFile)
import Prelude hiding (writeFile)

main :: IO ()
main = do
    putStr ""

    putStrLn ""
    putStr "Running ..."
    hFlush stdout
    u <- run
    putStrLn $ u `seq` " done."

    -- putStrLn ""
    -- putStr "Running v3 ..."
    -- hFlush stdout
    -- u3 <- runV3
    -- putStrLn $ u3 `seq` " done."

fileFrequencies :: FilePath
fileFrequencies = "deu_news_2020_freq.txt"

fileData :: FilePath
fileData = "german.utf8.dic"

fileSorted :: FilePath
fileSorted = "german.utf8.sorted.dic"

run :: IO ()
run = do
    mapFrequencies <- Map.fromList . parseFrequencies <$> BS.readFile fileFrequencies
    ls <- Char8.lines <$> BS.readFile fileData
    let sorted = sortOn (Down <<< \k -> Map.findWithDefault 0 k mapFrequencies) ls
    writeFile fileSorted $ foldMap ((<> "\n") . BSB.byteString) sorted

parseFrequencies :: ByteString -> [(ByteString, Int)]
parseFrequencies bs = case BS.uncons bs of
    Nothing -> []
    Just (w, _) | w == pound -> parseFrequencies $ BS.unsafeTail $ BS.dropWhile (/= linefeed) bs
    _ ->
        let (w, f) = BS.break (== tab) bs
         in case Char8.readInt (BS.unsafeTail f) of
                Just (i, bs') -> (w, i) : parseFrequencies (BS.unsafeTail bs')
                Nothing -> []
  where
    pound :: Word8
    pound = 35 -- '#'
    linefeed :: Word8
    linefeed = 10 -- '\n'
    tab :: Word8
    tab = 9 -- '\t'

-- using bytestring 0.10 and 0.11.1
-- `writeFile` is in bytestring 0.11.2
hPutBuilder :: Handle -> BSB.Builder -> IO ()
hPutBuilder h = BSB.hPut h . BSB.putBuilder

modifyFile :: IOMode -> FilePath -> BSB.Builder -> IO ()
modifyFile mode f bld = withBinaryFile f mode (`hPutBuilder` bld)

writeFile :: FilePath -> BSB.Builder -> IO ()
writeFile = modifyFile WriteMode
