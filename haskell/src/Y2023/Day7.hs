module Y2023.Day7 (solve) where

import AOC (getInput)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit)
import Data.List (nub, partition, sortBy)

data HandType = Five | Four | Full | Three | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord)

type Hand = (HandType, (String, Int))

eval :: (String, Int) -> (HandType, (String, Int))
eval h@(s@(a : b : c : _), bid)
  | length (nub s) == 1 = (Five, h)
  | length (nub s) == 5 = (HighCard, h)
  | length (nub s) == 4 = (OnePair, h)
  | length isA == 4 || length isB == 4 = (Four, h)
  | test == [3, 1] || test == [2, 1] = (Full, h)
  | 3 `elem` (length <$> [isA, isB, isC]) = (Three, h)
  | length (nub s) == 3 = (TwoPair, h)
  | otherwise = error "bad hand type eval"
  where
    (isA, notA) = partition (== a) s
    isB = filter (== b) s
    isC = filter (== c) s
    test = length <$> [isA, nub notA]

sndEval :: String -> String -> Ordering
sndEval (s1 : t1) (s2 : t2)
  | s1 == s2 = sndEval t1 t2
  | isAlpha s1 && isDigit s2 = GT
  | isAlpha s2 && isDigit s1 = LT
  | isDigit s1 && isDigit s2 = s1 `compare` s2
  | s1 == 'A' = GT
  | s2 == 'A' = LT
  | s1 == 'K' = GT
  | s2 == 'K' = LT
  | s1 == 'Q' = GT
  | s2 == 'Q' = LT
  | s1 == 'J' = GT
  | s2 == 'J' = LT
  | otherwise = error ("bad second level eval, " ++ " s1 " ++ show s1 ++ " s2 " ++ show s2)

comp :: Hand -> Hand -> Ordering
comp (t1, (h1, _)) (t2, (h2, _)) = (t2 `compare` t1) <> sndEval h1 h2

score :: [Hand] -> Int
score = sum . zipWith (*) [1 ..] . map (snd . snd)

p1 :: [(String, Int)] -> Int
p1 = score . sortBy comp . map eval

joker :: (HandType, (String, Int)) -> (HandType, (String, Int))
joker o@(t, h@(s, b)) = case t of
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
p2 = score . sortBy comp . map (f . eval)
  where
    f h@(t, (s, _)) = if '1' `elem` s then joker h else h

solve :: IO (Int, Int)
solve = do
  input <- map ((\(x : y : _) -> (x, read y :: Int)) . words) . lines <$> getInput "2023" "7"
  let input2 = map (first (map (\c -> if c == 'J' then '1' else c))) input -- using 1 for jokers means i don't have to change sndEval
  return (p1 input, p2 input2)
