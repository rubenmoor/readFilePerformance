{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Category ((<<<))
import Control.Monad.ST (runST)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import Data.Vector (Vector, freeze, thaw)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Tim as Tim
import System.IO (hFlush, stdout)
import Control.Monad ((<$!>))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import qualified Data.ByteString.Builder as BSB
import Data.List (sortOn)

main :: IO ()
main = do
    putStr ""

    putStr "Running v1 ..."
    hFlush stdout
    u1 <- runV1
    putStrLn $ u1 `seq` " done."

    putStrLn ""
    putStr "Running v2 ..."
    hFlush stdout
    u2 <- runV2
    putStrLn $ u2 `seq` " done."

    putStrLn ""
    putStr "Running v3 ..."
    hFlush stdout
    u3 <- runV3
    putStrLn $ u3 `seq` " done."

fileFrequencies :: FilePath
fileFrequencies = "deu_news_2020_freq.txt"

fileData :: FilePath
fileData = "german.utf8.dic"

fileSorted :: FilePath
fileSorted = "german.utf8.sorted.dic"

{- |
straightforward implementation, using Text-based IO
-}
runV1 :: IO ()
runV1 = do
    mapFrequencies <- readFrequencies
    ls <- (Lazy.toStrict <$!>) . Lazy.lines <$> Lazy.readFile fileData

    let
        vec = {-# SCC vec #-} Vector.fromList ({-# SCC ls #-} ls)
        sorted = quicksort mapFrequencies vec
    Lazy.writeFile fileSorted
        (Lazy.unlines $ Lazy.fromStrict <$> {-# SCC lsSorted #-} Vector.toList sorted)
  where
    {-# SCC readFrequencies #-}
    readFrequencies :: IO (HashMap Text Int)
    readFrequencies = do
        ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileFrequencies
        pure $ {-# SCC hmap #-} mkHashMap ({-# SCC ls #-} ls)

{- |
why not Lazy? read the file line by line, no need to hold it all in memory
-}
runV2 :: IO ()
runV2 = do
    mapFrequencies <- Map.fromList . parseFrequencies <$> BS.readFile fileFrequencies
    ls <- Char8.lines <$> BS.readFile fileData
    let sorted = sortOn (\k -> Map.findWithDefault 0 k mapFrequencies) ls
    BSB.writeFile fileSorted $ foldMap ((<> "\n") . BSB.byteString) sorted

{-|
trying to help with garbage collection, only making it worse
-}
runV3 :: IO ()
runV3 = do
    mapFrequencies <- Map.fromList . parseFrequencies <$> BS.readFile fileFrequencies
    ls <- Char8.lines <$> BS.readFile fileData
    let
        vec = {-# SCC vec #-} Vector.fromList ({-# SCC ls #-} ls)
        sorted = quicksort mapFrequencies vec
    BSB.writeFile fileSorted $ foldMap ((<> "\n") . BSB.byteString) sorted
  where
    readFrequencies :: IO (HashMap Text Int)
    readFrequencies = do
        ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileFrequencies
        pure $ {-# SCC hmap #-} mkHashMap ({-# SCC ls #-} ls)

freq :: HashMap Text Int -> Text -> Int
freq m w = HashMap.findWithDefault w m

quicksort ::
    HashMap Text Int -> Vector Text -> Vector Text
quicksort freqs vec = runST $ do
    mvec <- thaw vec
    Tim.sortBy (comparing $ Down <<< freq freqs) mvec
    freeze mvec

mkHashMap :: [Text] -> HashMap Text Int
mkHashMap ls =
    HashMap.fromList $
        catMaybes $
            ls <&> \l -> case Text.head l of
                '#' -> Nothing
                _ ->
                    let [w, f] = Text.splitOn "\t" l
                     in Just (w, read $ Text.unpack f)

parseFrequencies :: ByteString -> [(ByteString, Int)]
parseFrequencies bs = case BS.uncons bs of
    Nothing -> []
    Just (w, _) | w == pound -> parseFrequencies $ BS.unsafeTail $ BS.dropWhile (/= linefeed) bs
    _ -> let (w, f) = BS.break (== tab) bs in
         case Char8.readInt (BS.unsafeTail f) of
                Just (i, bs') -> (w, i) : parseFrequencies (BS.unsafeTail bs')
                Nothing -> []
  where
    pound :: Word8
    pound = 35 -- '#'

    linefeed :: Word8
    linefeed = 10 -- '\n'

    tab :: Word8
    tab = 9 -- '\t'
