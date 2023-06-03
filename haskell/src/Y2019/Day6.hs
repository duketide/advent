module Y2019.Day6 (solve) where

import AOC (getInput, readInt)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

tally :: Map String [String] -> String -> Int
tally mp = go 1
  where
    go :: Int -> String -> Int
    go mult lkup
      | null lst = 0
      | otherwise = (length lst * mult) + foldr (\x acc -> acc + go (mult + 1) x) 0 lst
      where
        lst = fromMaybe [] $ M.lookup lkup mp

bfs :: Map String [String] -> String -> String -> Int
bfs mp start end = go 0 (S.fromList (mp M.! start)) S.empty
  where
    go :: Int -> Set String -> Set String -> Int
    go n queue seen
      | S.member end nextQ = n
      | otherwise = go (n + 1) nextQ nextSeen
      where
        nextSeen = S.union queue seen
        nextQ = S.fromList $ foldr (\x acc -> acc ++ filter (\y -> not $ S.member y nextSeen) (mp M.! x)) [] queue

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "6"
  let orbitMap = foldr ((\(x : y : xs) mp -> M.insertWith (++) x [y] mp) . splitOn ")") M.empty $ lines rawInput
      p1 = tally orbitMap "COM"
      orbitGraph = foldr (\(k, vs) mp -> foldr (\v acc -> M.insertWith (++) v [k] acc) mp vs) orbitMap $ M.assocs orbitMap
      p2 = bfs orbitGraph "YOU" "SAN"
  return (p1, p2)
