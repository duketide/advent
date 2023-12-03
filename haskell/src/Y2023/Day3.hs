module Y2023.Day3 (solve) where

import AOC (Pair, getInput, nbrs)
import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as S

isDigOrDot :: Char -> Bool
isDigOrDot c = isDigit c || c == '.'

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
    syms = getSymbolCoords inp
    f :: (Int, Set Pair) -> Bool
    f (_, s) = not $ null $ S.intersection s syms

solve :: IO (Int, Int)
solve = do
  input <- lines <$> getInput "2023" "3"
  return (p1 input, 1)
