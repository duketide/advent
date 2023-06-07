module Y2019.Day8 (solve) where

import AOC (getInput, trim)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

col = 25

row = 6

layer = col * row

p1Solve :: String -> Int
p1Solve = fst . foldr next (0, (0, 0, 0, 1, layer))
  where
    next x (win, (zeroes, ones, twos, count, cMin))
      | isEnd = (nWin, (0, 0, 0, count + 1, nMin))
      | otherwise = (win, (z, o, t, count + 1, cMin))
      where
        isEnd = count `mod` layer == 0
        z = if x == '0' then zeroes + 1 else zeroes
        o = if x == '1' then ones + 1 else ones
        t = if x == '2' then twos + 1 else twos
        nMin = min cMin z
        nWin = if z < cMin then o * t else win

p2Map :: String -> IntMap Char
p2Map = snd . foldr (\x (ind, acc) -> (ind + 1, IM.insertWith (\n o -> if n == '2' then o else n) (ind `mod` layer) x acc)) (0, IM.empty)

p2Solve :: IntMap Char -> Maybe [String]
p2Solve im = chunksOf col . reverse . fmap f <$> mapM (`IM.lookup` im) [0 .. layer - 1]
  where
    f x = if x == '0' then ' ' else '#'

solve :: IO (Int, String)
solve = do
  input <- trim <$> getInput "2019" "8"
  let p1 = p1Solve input
      p2 = fromMaybe ["error", "in", "p2"] $ p2Solve $ p2Map input
  mapM_ putStrLn p2
  return (p1, "see print")
