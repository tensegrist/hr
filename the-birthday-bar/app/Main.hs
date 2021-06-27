{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}

module Main where

import Criterion.Main
import Data.Function
import Data.List
import Data.List.Split
import Data.Coerce
import Data.Text qualified as T
import System.Environment
import System.IO
import Test.QuickCheck

hrMain :: IO ()
hrMain = do
  let lstrip = T.unpack . T.stripStart . T.pack
  let rstrip = T.unpack . T.stripEnd . T.pack
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode
  nTemp <- getLine
  let n = read $ lstrip $ rstrip nTemp :: Int
  sTemp <- getLine
  let s = map (read :: String -> Int) . words $ rstrip sTemp
  firstMultipleInputTemp <- getLine
  let firstMultipleInput = words $ rstrip firstMultipleInputTemp
  let d = read (firstMultipleInput !! 0) :: Int
  let m = read (firstMultipleInput !! 1) :: Int
  let result = birthday s d m
  hPutStrLn fptr $ show result
  hFlush fptr
  hClose fptr

criterionMain :: IO ()
criterionMain =
  defaultMain
    [ bgroup "birthday0" $ mkBenches birthday0,
      bgroup "birthday1" $ mkBenches birthday1,
      bgroup "birthday2" $ mkBenches birthday2
    ]
  where
    n = 20
    xs = cycle [1 .. n]
    mkBench f len = bench (show len) $ nf (f (take len xs) (sum [1 .. n])) n
    mkBenches f = map (mkBench f) cases
    cases = [5000]

prop_birthday_is_sound :: ([Int] -> Int -> Int -> Int) -> NonEmptyList (NonNegative Int) -> Positive Int -> Positive Int -> Bool
prop_birthday_is_sound f (coerce -> s) (Positive d) (Positive m) = birthday0 s d m == f s d m

quickCheckMain = do
  quickCheck (withMaxSuccess 10000 (prop_birthday_is_sound birthday1))
  quickCheck (withMaxSuccess 10000 (prop_birthday_is_sound birthday2))

-- | nobrain/"reference"
birthday0 :: [Int] -> Int -> Int -> Int
birthday0 s d m = s & windows m & map sum & filter (== d) & length
  where
    windows n xs
      | length xs < n = []
      | otherwise = take n xs : windows n (tail xs)

-- | should probably use a fold
birthday1 :: [Int] -> Int -> Int -> Int
birthday1 xs0 target size = go 0 0 0 xs0
  where
    go _ _ count [] = count
    go ix wsum count (x : xs) = go (ix + 1) wsum' count' xs
      where
        wsum' = if ix >= size then wsum + x - (xs0 !! (ix - size)) else wsum + x
        count' = if ix + 1 >= size && wsum' == target then count + 1 else count

-- | eliminate the index by using another list that "lags" behind
birthday2 :: [Int] -> Int -> Int -> Int
birthday2 xs0 target size = go 0 0 0 xs0 xs0
  where
    go _ _ count [] _ = count
    go ix wsum count (x : xs) ys0@(y : ys) = go (ix + 1) wsum' count' xs ys'
      where
        (ys', wsum') = if ix >= size then (ys, wsum + x - y) else (ys0, wsum + x)
        count' = if ix + 1 >= size && wsum' == target then count + 1 else count

birthday = birthday2
-- main = quickCheckMain
main = criterionMain
-- main = hrMain
