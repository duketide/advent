module Y2019.Day14 (solve) where

import AOC (getInput, trim)
import Control.Monad (MonadPlus (mplus))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Tree = Map String Node

data Material = Material {compound :: String, quantity :: Int} deriving (Show)

data Node = Node {num :: Int, ingredients :: [Material]} deriving (Show)

maxOre = 10 ^ 12

solve :: IO (Int, Int)
solve = do
  input <- parse . fmap (fmap (splitOn ",") . splitOn "=>") . lines <$> getInput "2019" "14"
  return (dfs 1 input "FUEL", search (bounds 1 input) input)

parse :: [[[String]]] -> Tree
parse = foldr f M.empty
  where
    f (materials : (result : _) : _) = M.insert rName nd
      where
        (n : rName : _) = words . trim $ result
        nd = Node {num = read n, ingredients = foldr g [] materials}
          where
            g x acc = m : acc
              where
                (q : c : _) = words . trim $ x
                m = Material {compound = c, quantity = read q}

dfs :: Int -> Tree -> String -> Int
dfs n t s = go [(s, n)] M.empty 0
  where
    go :: [(String, Int)] -> Map String Int -> Int -> Int
    go [] mp count = count
    go ((x, y) : xs) mp count
      | x == "ORE" = go xs mp (count + y)
      | otherwise = go nLst nMp count
      where
        needed = y - offset
        curr = fromMaybe 0 $ M.lookup x mp
        offset = min y curr
        m = ceiling $ fromIntegral needed / fromIntegral num
        excess = m * num - needed
        nMp = M.insert x (curr - offset + excess) mp
        ys = fmap (\Material {compound = c, quantity = q} -> (c, m * q)) ing
        nLst = ys ++ xs
        Node {num = num, ingredients = ing} = t M.! x

bounds :: Int -> Tree -> (Int, Int)
bounds n t
  | result > maxOre = (n, n `div` 2)
  | otherwise = bounds (n * 2) t
  where
    result = dfs n t "FUEL"

search :: (Int, Int) -> Tree -> Int
search (upper, lower) tree
  | mid > maxOre && subMid <= maxOre = n - 1
  | otherwise = search (upper', lower') tree
  where
    n = (upper + lower) `div` 2
    mid = dfs n tree "FUEL"
    subMid = dfs (n - 1) tree "FUEL"
    upper' = if mid > maxOre then n else upper
    lower' = if mid < maxOre then n else lower
