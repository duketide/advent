{-# LANGUAGE OverloadedStrings #-}

module AOC (readInt, getInput, trim, rawLines) where

import Control.Lens ((&), (.~), (^.))
import Data.ByteString (ByteString)
import Data.Char (isSpace, readLitChar)
import Data.List.Split (splitOn)
import Network.Wreq (defaults, getWith, header, responseBody)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Posix.Env.ByteString (getEnv)

readInt :: String -> Int
readInt = read

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseString :: String -> String
parseString s = go s []
  where
    go :: String -> String -> String
    go [] r = reverse r
    go o r = go p (next : r)
      where
        (next, p) = head $ readLitChar o

rawLines :: String -> [String]
rawLines = splitOn "\\n"

getInput :: String -> String -> IO String
getInput year day = do
  home <- getHomeDirectory
  let file = home ++ "/coding/advent/input/" ++ year ++ "/day" ++ day ++ ".txt"
  haveInput <- doesFileExist file
  if haveInput
    then readFile file
    else do
      -- need to store "AOC_SESSION" as "session={token}"
      token <- getEnv "AOC_SESSION"
      case token of
        Nothing -> return "busted env variable"
        Just x -> fetch x year day file

fetch :: ByteString -> String -> String -> String -> IO String
fetch token year day file = do
  home <- getHomeDirectory
  let opts = defaults & header "Cookie" .~ [token]
  response <- getWith opts ("https://adventofcode.com/" ++ year ++ "/day/" ++ day ++ "/input")
  let str = parseString $ tail . init . show $ response ^. responseBody
  writeFile file str
  readFile file
