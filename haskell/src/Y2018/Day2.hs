module Y2018.Day2 (solve) where

import AOC (getInput)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

counter :: String -> Map Char Int
counter = foldr (\c acc -> M.insert c (fromMaybe 0 (M.lookup c acc) + 1) acc) M.empty

twoOrThree :: Map Char Int -> (Bool, Bool)
twoOrThree mp
  | 2 `elem` vals && 3 `elem` vals = (True, True)
  | 2 `elem` vals = (True, False)
  | 3 `elem` vals = (False, True)
  | otherwise = (False, False)
  where
    vals = M.elems mp

p1 :: [String] -> (Int, Int)
p1 = foldr folder (0, 0)
  where
    folder l (twos, threes) = (twos + if two' then 1 else 0, threes + if three' then 1 else 0)
      where
        (two', three') = twoOrThree $ counter l

diffCounter :: String -> String -> Bool
diffCounter = go 0
  where
    go :: Int -> String -> String -> Bool
    go 1 [] _ = True
    go 1 _ [] = True
    go 2 _ _ = False
    go n (l1 : s1) (l2 : s2) = go (n + if l1 == l2 then 0 else 1) s1 s2
    go _ _ _ = error "bad diffCounter"

multiDiff :: [String] -> Maybe (String, String)
multiDiff (x : xs) = go x xs
  where
    go :: String -> [String] -> Maybe (String, String)
    go test [] = Nothing
    go test (y : ys)
      | diffCounter test y = Just (test, y)
      | otherwise = go test ys

p2finder :: [String] -> (String, String)
p2finder [] = error "p2finder error"
p2finder x = case multiDiff x of
  Nothing -> p2finder $ tail x
  Just (x, y) -> (x, y)

sift :: (String, String) -> String
sift (s1, s2) = go s1 s2 []
  where
    go [] _ s = reverse s
    go _ [] s = reverse s
    go (x : xs) (y : ys) s
      | x == y = go xs ys (x : s)
      | otherwise = go xs ys s

p2 :: [String] -> String
p2 = sift . p2finder

solve :: IO (Int, String)
solve = do
  input <- lines <$> getInput "2018" "2"
  let (p1twos, p1threes) = p1 input
  return (p1twos * p1threes, p2 input)
