module Y2023.Day3 (solve) where

import AOC (Pair, getInput, nbrs)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

isDigOrDot :: Char -> Bool
isDigOrDot c = isDigit c || c == '.'

isGear :: Char -> Bool
isGear c = c == '*'

getSymbolCoords :: [String] -> Set Pair
getSymbolCoords = snd . foldl lineFolder ((0, -1), S.empty)

lineFolder :: (Pair, Set Pair) -> String -> (Pair, Set Pair)
lineFolder ((_, y), s) = foldl pointChecker ((0, y + 1), s)

pointChecker :: (Pair, Set Pair) -> Char -> (Pair, Set Pair)
pointChecker (p@(x, y), s) ch
  | isDigOrDot ch = (next, s)
  | otherwise = (next, S.insert p s)
  where
    next = (x + 1, y)

getGearCoords :: [String] -> Set Pair
getGearCoords = snd . foldl gearFolder ((0, -1), S.empty)

gearFolder :: (Pair, Set Pair) -> String -> (Pair, Set Pair)
gearFolder ((_, y), s) = foldl gearChecker ((0, y + 1), s)

gearChecker :: (Pair, Set Pair) -> Char -> (Pair, Set Pair)
gearChecker (p@(x, y), s) ch
  | isGear ch = (next, S.insert p s)
  | otherwise = (next, s)
  where
    next = (x + 1, y)

numsAndNbrs :: [String] -> [(Int, Set Pair)]
numsAndNbrs = snd . foldl lineRecurser (0, [])

tallyNbrs :: [(Int, Set Pair)] -> Map Pair [Int]
tallyNbrs = foldr singleTally M.empty

singleTally :: (Int, Set Pair) -> Map Pair [Int] -> Map Pair [Int]
singleTally (num, s) mp = S.foldr func mp s
  where
    func pt acc = M.insert pt (num : fromMaybe [] (M.lookup pt acc)) acc

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
    syms = getSymbolCoords inp
    f :: (Int, Set Pair) -> Bool
    f (_, s) = not $ null $ S.intersection s syms

p2 :: [String] -> Int
p2 inp = S.foldr func 0 gears
  where
    tally = tallyNbrs . numsAndNbrs $ inp
    gears = getGearCoords inp
    func p accum
      | length l == 2 = accum + (head l * l !! 1)
      | otherwise = accum
      where
        l = fromMaybe [] $ M.lookup p tally

solve :: IO (Int, Int)
solve = do
  input <- lines <$> getInput "2023" "3"
  return (p1 input, p2 input)
