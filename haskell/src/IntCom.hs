module IntCom
  ( execute,
    findRes,
    loadProg,
    Return (R),
    status,
    state,
    outputs,
    HaltOrAwait (Halt, Await),
  )
where

import Data.Map (Map)
import qualified Data.Map as M

data HaltOrAwait = Halt | Await deriving (Eq, Ord, Show)

data Return = R
  { status :: HaltOrAwait,
    state :: (Int, Map Int Int), -- the Int is the instruction pointer
    outputs :: [Int]
  }

padLength = 5

pad :: String -> String
pad s
  | length s == padLength = s
  | otherwise = pad ('0' : s)

execute :: Int -> [Int] -> Map Int Int -> Return
execute start inp = go inp [] start
  where
    go :: [Int] -> [Int] -> Int -> Map Int Int -> Return
    go i o pos m
      | opCode == 99 = R {status = Halt, state = (pos, m), outputs = o}
      | opCode == 1 = go i o (pos + 4) $ M.insert three sm m
      | opCode == 2 = go i o (pos + 4) $ M.insert three pd m
      | opCode == 3 = if null i then R {status = Await, state = (pos, m), outputs = o} else go (tail i) o (pos + 2) $ M.insert one (head i) m
      | opCode == 4 = go i (m M.! one : o) (pos + 2) m
      | opCode == 5 = go i o (if isTrue then m M.! two else pos + 3) m
      | opCode == 6 = go i o (if isFalse then m M.! two else pos + 3) m
      | opCode == 7 = go i o (pos + 4) lessThan
      | opCode == 8 = go i o (pos + 4) equals
      | otherwise = error ("bad opCode of " ++ show opCode ++ " at pos " ++ show pos ++ " which is " ++ show (m M.! pos))
      where
        inst = pad $ show $ m M.! pos
        opCode = read $ drop (padLength - 2) inst
        one = f pOne (pos + 1)
        two = f pTwo (pos + 2)
        three = f pThree (pos + 3)
        pOne = inst !! max 0 (padLength - 3)
        pTwo = inst !! max 0 (padLength - 4)
        pThree = inst !! max 0 (padLength - 5)
        sm = m M.! one + m M.! two
        pd = m M.! one * m M.! two
        isTrue = m M.! one /= 0
        isFalse = m M.! one == 0
        lessThan = M.insert three (if m M.! one < m M.! two then 1 else 0) m
        equals = M.insert three (if m M.! one == m M.! two then 1 else 0) m
        f p
          | p == '1' = id
          | otherwise = (M.!) m

findRes :: Int -> Map Int Int -> Int
findRes res = go 99 99
  where
    go noun verb mp
      | answer == res = 100 * noun + verb
      | verb == -1 = go (noun - 1) 99 mp
      | otherwise = go noun (verb - 1) mp
      where
        ansMap = snd . state $ execute 0 [] $ M.insert 2 verb (M.insert 1 noun mp)
        answer = ansMap M.! 0

loadProg :: [Int] -> Map Int Int
loadProg = fst . foldl (\(m, n) x -> (M.insert n x m, n + 1)) (M.empty, 0)
