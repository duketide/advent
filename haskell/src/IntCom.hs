module IntCom
  ( execute,
    program,
    findRes,
    loadProg,
    Return (R),
    status,
    state,
    outputs,
    HaltOrAwait (Halt, Await),
    Program,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)

type Program = IntMap Int

data HaltOrAwait = Halt | Await deriving (Eq, Ord, Show)

data Return = R
  { status :: HaltOrAwait,
    state :: (Int, Program), -- the Int is the instruction pointer
    outputs :: [Int]
  }

padLength = 5

pad :: String -> String
pad s
  | length s == padLength = s
  | otherwise = pad ('0' : s)

execute :: Int -> [Int] -> Program -> Return
execute start inp = go inp [] start
  where
    go :: [Int] -> [Int] -> Int -> Program -> Return
    go i o pos m = case opCode of
      99 -> R {status = Halt, state = (pos, m), outputs = o}
      1 -> go i o (pos + 4) $ IM.insert three sm m
      2 -> go i o (pos + 4) $ IM.insert three pd m
      3 -> if null i then R {status = Await, state = (pos, m), outputs = o} else go (tail i) o (pos + 2) $ IM.insert one (head i) m
      4 -> go i (m IM.! one : o) (pos + 2) m
      5 -> go i o (if isTrue then m IM.! two else pos + 3) m
      6 -> go i o (if isFalse then m IM.! two else pos + 3) m
      7 -> go i o (pos + 4) lessThan
      8 -> go i o (pos + 4) equals
      _ -> error ("bad opCode of " ++ show opCode ++ " at pos " ++ show pos ++ " which is " ++ show (m IM.! pos))
      where
        inst = pad $ show $ m IM.! pos
        opCode = read $ drop (padLength - 2) inst
        one = f pOne (pos + 1)
        two = f pTwo (pos + 2)
        three = f pThree (pos + 3)
        pOne = inst !! max 0 (padLength - 3)
        pTwo = inst !! max 0 (padLength - 4)
        pThree = inst !! max 0 (padLength - 5)
        sm = m IM.! one + m IM.! two
        pd = m IM.! one * m IM.! two
        isTrue = m IM.! one /= 0
        isFalse = m IM.! one == 0
        lessThan = IM.insert three (if m IM.! one < m IM.! two then 1 else 0) m
        equals = IM.insert three (if m IM.! one == m IM.! two then 1 else 0) m
        f p
          | p == '1' = id
          | otherwise = (IM.!) m

findRes :: Int -> Program -> Int
findRes res = go 99 99
  where
    go noun verb mp
      | answer == res = 100 * noun + verb
      | verb == -1 = go (noun - 1) 99 mp
      | otherwise = go noun (verb - 1) mp
      where
        ansMap = snd . state $ execute 0 [] $ IM.insert 2 verb (IM.insert 1 noun mp)
        answer = ansMap IM.! 0

program :: String -> Program
program l = loadProg $ read <$> splitOn "," l

loadProg :: [Int] -> Program
loadProg = fst . foldl (\(m, n) x -> (IM.insert n x m, n + 1)) (IM.empty, 0)
