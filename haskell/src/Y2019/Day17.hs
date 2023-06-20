module Y2019.Day17 (solve) where

import AOC (getInput)
import Data.Char (ord)
import qualified Data.IntMap as IM
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as S
import IntCom (Program, Return (outputs), execute, program)

type Point = (Int, Int)

data Turn = R Int | L Int

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "17"
  -- hard-coded my p2 solution based on printing out the map
  let mainRoutine = ord <$> "A,B,B,C,C,A,B,B,C,A\n"
      a = ord <$> "R,4,R,12,R,10,L,12\n"
      b = ord <$> "L,12,R,4,R,12\n"
      c = ord <$> "L,12,L,8,R,10\n"
      inp = mainRoutine ++ a ++ b ++ c ++ [ord 'n', 10]
      input2 = IM.insert 0 2 input
      (p2 : _) = outputs . execute 0 inp 0 $ input2
  return (p1 input, p2)

p1 :: Program -> Int
p1 = sum . tally . S.toList . filt . makeSet . outputs . execute 0 [] 0
  where
    tally = map (uncurry (*))
    filt s = S.filter f s
      where
        f (x, y) = and $ (`S.member` s) <$> [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

makeSet :: [Int] -> Set Point
makeSet = fst . foldr f (S.empty, (0, 0))
  where
    f tile (st, (x, y)) = (nextSt, (nextX, nextY))
      where
        nextSt = if tile == 35 then S.insert (x, y) st else st
        (nextX, nextY) = if tile == 10 then (0, y + 1) else (x + 1, y)
