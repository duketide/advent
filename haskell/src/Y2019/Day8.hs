module Y2019.Day8 (solve) where

import AOC (getInput, trim)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

col = 25

row = 6

layer = col * row

p1Solve :: String -> (Int, Int, Int, Int, Int, Int)
p1Solve = foldr next (0, 0, 0, 1, layer, 0)
  where
    next x (zeroes, ones, twos, count, cMin, win) = (nZero, nOne, nTwo, nCount, nMin, nWin)
      where
        nCount = count + 1
        isEnd = count `mod` layer == 0
        isZero = x == '0'
        isOne = x == '1'
        isTwo = x == '2'
        z = if isZero then zeroes + 1 else zeroes
        nZero = if isEnd then 0 else z
        o = if isOne then ones + 1 else ones
        nOne = if isEnd then 0 else o
        t = if isTwo then twos + 1 else twos
        nTwo = if isEnd then 0 else t
        nMin = if isEnd then min cMin z else cMin
        nWin = if isEnd && z < cMin then o * t else win

p2Map :: String -> IntMap Char
p2Map s = snd $ foldr (\x (ind, acc) -> (ind + 1, IM.insertWith (\n o -> if n == '2' then o else n) (ind `mod` layer) x acc)) (0, IM.empty) s

p2Solve :: IntMap Char -> Maybe [String]
p2Solve im = chunksOf col . reverse . fmap f <$> mapM (`IM.lookup` im) [0 .. layer - 1]
  where
    f x = if x == '0' then ' ' else '#'

solve :: IO (Int, String)
solve = do
  input <- trim <$> getInput "2019" "8"
  let (_, _, _, _, _, p1) = p1Solve input
      p2 = fromMaybe ["error", "in", "p2"] $ p2Solve $ p2Map input
  mapM_ putStrLn p2
  return (p1, "see print")
