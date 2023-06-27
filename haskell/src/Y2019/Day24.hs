{-# LANGUAGE TupleSections #-}

module Y2019.Day24 (solve) where

import AOC (getInput, nbrs4, nbrsBounded4)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

type RPoint = ((Int, Int), Int)

top = (,0) <$> [0 .. 4]

bottom = (,4) <$> [0 .. 4]

right = (4,) <$> [0 .. 4]

left = (0,) <$> [0 .. 4]

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "24"
  return (p1 input, p2 input)

score :: Map Point Char -> Int
score = foldr (\((x, y), ch) acc -> if ch == '.' then acc else acc + 2 ^ (5 * y + x)) 0 . M.assocs

p1 :: String -> Int
p1 = score . findDup . parse

findDup :: Map Point Char -> Map Point Char
findDup = go S.empty
  where
    go seen mp
      | S.member mp seen = mp
      | otherwise = go (S.insert mp seen) (turn mp)

turn :: Map Point Char -> Map Point Char
turn mp = foldr f M.empty . M.assocs $ mp
  where
    f ((x, y), ch) = M.insert (x, y) val
      where
        ns = nbrsBounded4 (0, 4) (0, 4) (x, y)
        ct = foldr (\x acc -> if M.lookup x mp == Just '#' then acc + 1 else acc) 0 ns
        val = case ct of
          1 -> '#'
          2 -> if ch == '.' then '#' else '.'
          _ -> '.'

parse :: String -> Map Point Char
parse = snd . foldl f ((0, 0), M.empty)
  where
    f ((x, y), mp) '\n' = ((0, y + 1), mp)
    f ((x, y), mp) ch = ((x + 1, y), M.insert (x, y) ch mp)

p2 :: String -> Int
p2 str = go 0 (parse2 str)
  where
    go 200 st = length st
    go n st = go (n + 1) (turn2 st)

turn2 :: Set RPoint -> Set RPoint
turn2 s = foldr f S.empty . M.assocs $ mp
  where
    mp = tally s
    f (rpt@((x, y), lvl), ct) acc
      | ct == 1 = S.insert rpt acc
      | ct == 2 && not (S.member rpt s) = S.insert rpt acc
      | otherwise = acc

tally :: Set RPoint -> Map RPoint Int
tally = foldr f M.empty
  where
    f ((x, y), lvl) acc = foldr (\rpt acc' -> M.insertWith (+) rpt 1 acc') acc ns
      where
        ns' = nbrs4 (x, y)
        ns = ns' >>= lvlCheck
        lvlCheck (-1, _) = [((1, 2), lvl - 1)]
        lvlCheck (5, _) = [((3, 2), lvl - 1)]
        lvlCheck (_, 5) = [((2, 3), lvl - 1)]
        lvlCheck (_, -1) = [((2, 1), lvl - 1)]
        lvlCheck (2, 2) = case (x, y) of
          (2, 1) -> (,lvl + 1) <$> top
          (2, 3) -> (,lvl + 1) <$> bottom
          (1, 2) -> (,lvl + 1) <$> left
          (3, 2) -> (,lvl + 1) <$> right
        lvlCheck other = [(other, lvl)]

parse2 :: String -> Set RPoint
parse2 = snd . foldl f ((0, 0), S.empty)
  where
    f ((x, y), st) '\n' = ((0, y + 1), st)
    f ((x, y), st) '#' = ((x + 1, y), S.insert ((x, y), 0) st)
    f ((x, y), st) _ = ((x + 1, y), st)
