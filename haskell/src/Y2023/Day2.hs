module Y2023.Day2 (solve) where

import AOC (getInput)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Game = Game
  { game :: Int,
    red :: Int,
    blue :: Int,
    green :: Int
  }
  deriving (Show)

gameParse :: [String] -> Game
gameParse [] = error "empty game"
gameParse (x : xs) = Game {game = read (init x), red = red, blue = blue, green = green}
  where
    counts = chunksOf 2 xs
    foldedCounts = foldr inputFolder M.empty counts
    red = fromMaybe 0 $ M.lookup "red" foldedCounts
    blue = fromMaybe 0 $ M.lookup "blue" foldedCounts
    green = fromMaybe 0 $ M.lookup "green" foldedCounts

inputFolder :: [String] -> Map String Int -> Map String Int
inputFolder (x : y : _) mp = case y of
  ('b' : _) -> M.insertWith max "blue" (read x) mp
  ('r' : _) -> M.insertWith max "red" (read x) mp
  ('g' : _) -> M.insertWith max "green" (read x) mp
  _ -> error "no color"
inputFolder _ _ = error "bad inputFolder"

p1Tally :: Game -> Int -> Int
p1Tally g n
  | red g <= 12 && blue g <= 14 && green g <= 13 = n + game g
  | otherwise = n

p1 :: [Game] -> Int
p1 = foldr p1Tally 0

p2Tally :: Game -> Int -> Int
p2Tally g n = n + (red g * blue g * green g)

p2 :: [Game] -> Int
p2 = foldr p2Tally 0

solve :: IO (Int, Int)
solve = do
  rawInput <- map (tail . words) . lines <$> getInput "2023" "2"
  let input = gameParse <$> rawInput
  return (p1 input, p2 input)
