module Y2019.Day4 (solve) where

import AOC (getInput, readInt)
import Data.Char (chr, ord)
import Data.List.Split

data Part = P1 | P2 deriving (Eq)

startCh = chr (ord '0' - 1)

test :: Int -> Bool
test n = f $ foldl (\(nondec, doub, prev) x -> (nondec && x >= prev, doub || x == prev, x)) (True, False, startCh) $ show n
  where
    f (a, b, _) = a && b

test' :: Int -> Bool
test' n = ans $ foldl f (True, False, False, False, startCh) $ show n
  where
    -- a shows whether we've been nondecreasing throughout, b shows whether we ended on a double, c shows whether a double was established earlier
    ans (a, b, c, _, _) = a && (b || c)
    f st x = (nextNonDec, nextOnDoub, nextDoubEst, nextOnStreak, x)
      where
        (nondec, onDoub, doubEst, onStreak, prev) = st
        nextNonDec = nondec && x >= prev
        nextOnDoub = not onDoub && not onStreak && x == prev
        nextDoubEst = doubEst || (onDoub && x /= prev)
        nextOnStreak = x == prev && (onDoub || onStreak)

tally :: Int -> Int -> Part -> Int
tally start stop p = go 0 start
  where
    go res curr
      | curr == stop = res
      | otherwise = go (res + t) (curr + 1)
      where
        t = if f curr then 1 else 0
        f = if p == P1 then test else test'

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "4"
  let start = head nums
      stop = last nums + 1
      nums = map readInt $ splitOn "-" rawInput
      p1 = tally start stop P1
      p2 = tally start stop P2
  return (p1, p2)
