module Y2023.Day8 (solve) where

import AOC (getInput)
import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

type NodeMap = Map String (String, String)

parse :: [String] -> (String, NodeMap)
parse (lr : mp : _) = (lr, foldr lineMap M.empty $ lines mp)
parse _ = error "parse error"

lineMap :: String -> NodeMap -> NodeMap
lineMap x = M.insert nd (l, r)
  where
    (nd : nds : _) = splitOn " = " x
    (l : r : _) = filter isAlpha <$> words nds

walker :: Char -> String -> NodeMap -> String
walker c s nm = f (nm M.! s)
  where
    f = case c of
      'L' -> fst
      'R' -> snd
      _ -> error "walking"

p1 :: (String, NodeMap) -> Int
p1 (lr, mp) = go lr 0 "AAA"
  where
    go :: String -> Int -> String -> Int
    go _ c "ZZZ" = c
    go (dir : dirs) c nd = go (dirs ++ [dir]) (c + 1) (walker dir nd mp)

aFinder :: NodeMap -> [String]
aFinder = filter (\x -> last x == 'A') . M.keys

zFinder :: String -> NodeMap -> String -> Int
zFinder o@(lr : lrs) nm nd = go 1 (lrs ++ [lr]) (walker lr nd nm)
  where
    go :: Int -> String -> String -> Int
    go c p@(dir : dirs) n
      | last n == 'Z' = c
      | otherwise = go (c + 1) (dirs ++ [dir]) (walker dir n nm)

{- for part 2, i empirically tested that the first two final Zs for
   each starting node were x and 2x, so it looked like the number of
   steps to the first Z was also a cycle. i'm not sure that had to be
   true. -}

p2 :: (String, NodeMap) -> Int
p2 (lr, mp) = foldr lcm 1 $ zFinder lr mp <$> aFinder mp

solve :: IO (Int, Int)
solve = do
  input <- parse . splitOn "\n\n" <$> getInput "2023" "8"
  return (p1 input, p2 input)
