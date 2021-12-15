{-# LANGUAGE OverloadedStrings #-}

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
import Data.Vector.Algorithms.Intro (sortBy)
import System.IO (hFlush, stdout)

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
    ls <- Text.lines <$> Text.readFile fileData
    let sorted = quicksort mapFrequencies $ Vector.fromList ls
    Text.writeFile fileSorted $ Text.unlines $ Vector.toList sorted
  where
    readFrequencies :: IO (HashMap Text Int)
    readFrequencies = do
        ls <- Text.lines <$> Text.readFile fileFrequencies
        pure $ mkHashMap ls

{- |
why not Lazy? read the file line by line, no need to hold it all in memory
-}
runV2 :: IO ()
runV2 = do
    mapFrequencies <- readFrequencies
    ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileData

    let
        -- alternatives:
        --     Vector.fromListN (length ls) ls
        --     Vector.generate (length ls) $ \i -> ls !! i
        vec = Vector.fromList ls

        -- the idea: ls can get garbage-collected
        sorted = vec `seq` quicksort mapFrequencies vec

    -- I don't know what the lazy version of writeFile does differently
    Lazy.writeFile fileSorted $ Lazy.unlines $ Lazy.fromStrict <$> Vector.toList sorted
  where
    readFrequencies :: IO (HashMap Text Int)
    readFrequencies = do
        ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileFrequencies
        pure $ mkHashMap ls

{- |
Lazy IO apparently implies that files are longer in use than needed
-}
runV3 :: IO ()
runV3 = do
    mapFrequencies <- readFrequencies
    ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileData

    let
        vec = Vector.fromList ls

        -- the idea: ls can get garbage-collected, but ...
        sorted = vec `seq` quicksort mapFrequencies vec

    -- ... fileData is still locked ¯\_(ツ)_/¯
    Lazy.writeFile fileData $ Lazy.unlines $ Lazy.fromStrict <$> Vector.toList sorted
  where
    readFrequencies :: IO (HashMap Text Int)
    readFrequencies = do
        ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileFrequencies
        pure $ mkHashMap ls


freq :: HashMap Text Int -> Text -> Int
freq m w = fromMaybe 0 $ HashMap.lookup w m

quicksort ::
    HashMap Text Int -> Vector Text -> Vector Text
quicksort freqs vec = runST $ do
    mvec <- thaw vec
    sortBy (comparing $ Down <<< freq freqs) mvec
    freeze mvec

mkHashMap :: [Text] -> HashMap Text Int
mkHashMap ls =
    HashMap.fromList $ catMaybes $ ls <&> \l -> case Text.head l of
                            '#' -> Nothing
                            _ -> let [w, f] = Text.splitOn "\t" l
                                 in Just (w, read $ Text.unpack f)
