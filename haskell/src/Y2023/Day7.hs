module Y2023.Day7 (solve) where

import AOC (getInput)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit)
import Data.List (nub, partition, sortBy)

data HandType = Five | Four | Full | Three | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord)

eval :: (String, Int) -> (HandType, (String, Int))
eval h@(s@(a : b : c : d : e : _), bid)
  | length (nub s) == 1 = (Five, h)
  | length (nub s) == 5 = (HighCard, h)
  | length isA == 4 = (Four, h)
  | length (filter (== b) s) == 4 = (Four, h)
  | test == [3, 1, 1] = (Full, h)
  | test == [2, 1, 1] = (Full, h)
  | test == [3, 1, 2] = (Three, h)
  | test == [2, 2, 1] = (Three, h)
  | test == [1, 1, 2] && (length headNotA == 3 || length lastNotA == 3) = (Three, h)
  | length (nub s) == 4 = (OnePair, h)
  | length (nub s) == 3 = (TwoPair, h)
  | otherwise = error "bad eval"
  where
    (isA, notA) = partition (== a) s
    headNotA = filter (== head notA) notA
    lastNotA = filter (== last notA) notA
    test = length <$> [isA, nub isA, nub notA]

sndEval :: String -> String -> Ordering
sndEval (s1 : t1) (s2 : t2)
  | s1 == s2 = sndEval t1 t2
  | isAlpha s1 && isDigit s2 = GT
  | isAlpha s2 && isDigit s1 = LT
  | isDigit s1 && isDigit s2 = compare s1 s2
  | s1 == 'A' = GT
  | s2 == 'A' = LT
  | s1 == 'K' = GT
  | s2 == 'K' = LT
  | s1 == 'Q' = GT
  | s2 == 'Q' = LT
  | s1 == 'J' = GT
  | s2 == 'J' = LT
  | otherwise = error ("s1" ++ show s1 ++ "s2" ++ show s2)

comp :: (HandType, (String, Int)) -> (HandType, (String, Int)) -> Ordering
comp (t1, (h1, _)) (t2, (h2, _)) = (t2 `compare` t1) <> sndEval h1 h2

p1 :: [(String, Int)] -> Int
p1 inp = sum scores'
  where
    scores = zip [1 ..] sorted
    sorted = sortBy comp initList
    initList = eval <$> inp
    scores' = map (\(rank, (_, (_, bid))) -> rank * bid) scores

jMapper :: (HandType, (String, Int)) -> (HandType, (String, Int))
jMapper o@(t, h@(s, b)) = case t of
  Five -> o
  Four -> (Five, h)
  Full -> (Five, h)
  Three -> (Four, h)
  OnePair -> (Three, h)
  HighCard -> (OnePair, h)
  TwoPair -> if test then (Four, h) else (Full, h)
  where
    test = length (filter (== '1') s) == 2

p2 :: [(String, Int)] -> Int
p2 inp = sum scores'
  where
    scores = zip [1 ..] sorted
    sorted = sortBy comp jSorted
    jSorted = map f initList
    f h@(t, (s, b)) = if '1' `elem` s then jMapper h else h
    initList = eval <$> inp
    scores' = map (\(rank, (_, (_, bid))) -> rank * bid) scores

solve :: IO (Int, Int)
solve = do
  input <- map ((\(x : y : _) -> (x, read y :: Int)) . words) . lines <$> getInput "2023" "7"
  let input2 = map (first (map (\c -> if c == 'J' then '1' else c))) input
  return (p1 input, p2 input2)
