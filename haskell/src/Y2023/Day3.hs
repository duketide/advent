module Y2023.Day3 (solve) where

import AOC (Pair, getInput, nbrs)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

notDigOrDot :: Char -> Bool
notDigOrDot c = not (isDigit c || c == '.')

getCoords :: ((Pair, Set Pair) -> Char -> (Pair, Set Pair)) -> [String] -> Set Pair
getCoords f = snd . foldl (folder f) ((0, -1), S.empty)

folder :: ((Pair, Set Pair) -> Char -> (Pair, Set Pair)) -> (Pair, Set Pair) -> String -> (Pair, Set Pair)
folder f ((_, y), s) = foldl f ((0, y + 1), s)

checker :: (Char -> Bool) -> (Pair, Set Pair) -> Char -> (Pair, Set Pair)
checker f (p@(x, y), s) ch
  | f ch = (next, S.insert p s)
  | otherwise = (next, s)
  where
    next = (x + 1, y)

numsAndNbrs :: [String] -> [(Int, Set Pair)]
numsAndNbrs = snd . foldl lineRecurser (0, [])

lineRecurser :: (Int, [(Int, Set Pair)]) -> String -> (Int, [(Int, Set Pair)])
lineRecurser (y, acc) = go ((0, y), [], S.empty, acc)
  where
    go :: (Pair, String, Set Pair, [(Int, Set Pair)]) -> String -> (Int, [(Int, Set Pair)])
    go ((_, y), c, ns, res) []
      | null c = (y + 1, res)
      | otherwise = (y + 1, (read $ reverse c, ns) : res)
    go ((x, y), c, ns, res) (h : t)
      | not (isDigit h) && null c = go ((x + 1, y), c, S.empty, res) t
      | not $ isDigit h = go ((x + 1, y), [], S.empty, (read $ reverse c, ns) : res) t
      | otherwise = go ((x + 1, y), h : c, newNs, res) t
      where
        newNs = S.union ns (S.fromList $ nbrs (x, y))

p1 :: [String] -> Int
p1 inp = sum . map fst . filter f . numsAndNbrs $ inp
  where
    syms = getCoords (checker notDigOrDot) inp
    f :: (Int, Set Pair) -> Bool
    f (_, s) = not $ null $ S.intersection s syms

tallyNbrs :: [(Int, Set Pair)] -> Map Pair [Int]
tallyNbrs = foldr singleTally M.empty

singleTally :: (Int, Set Pair) -> Map Pair [Int] -> Map Pair [Int]
singleTally (num, s) mp = S.foldr func mp s
  where
    func pt acc = M.insert pt (num : fromMaybe [] (M.lookup pt acc)) acc

p2 :: [String] -> Int
p2 inp = S.foldr func 0 gears
  where
    tally = tallyNbrs . numsAndNbrs $ inp
    gears = getCoords (checker (== '*')) inp
    func p accum
      | length l == 2 = accum + product l
      | otherwise = accum
      where
        l = fromMaybe [] $ M.lookup p tally

solve :: IO (Int, Int)
solve = do
  input <- lines <$> getInput "2023" "3"
  return (p1 input, p2 input)
