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

solve :: IO (Int, Int)
solve = do
  input <- parse . fmap (fmap (splitOn ",") . splitOn "=>") . lines <$> getInput "2019" "14"
  return (dfs 1 input "FUEL", p2 2391000 input "FUEL")

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

-- could speed up p2 with binary search
p2 :: Int -> Tree -> String -> Int
p2 n t s
  | dfs n t s <= 1000000000000 = n
  | otherwise = p2 (n - 1) t s

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
