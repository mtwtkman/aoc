module Main where

import Data.Functor ((<&>))
import System.Environment (getArgs)

splitBy :: Char -> String -> [String]
splitBy d = go ""
 where
  go :: String -> String -> [String]
  go acc [x] = if x == d then [acc] else [acc <> [x]]
  go acc (x : xs) = if x == d then [acc] <> go "" xs else go (acc <> [x]) xs
  go _ "" = []

getFilePath :: IO FilePath
getFilePath = getArgs <&> head

type SourceOrderRule = [(Int, Int)]
type SourceUpdate = [[Int]]

data Source = Source {
  orderRule :: SourceOrderRule,
  update :: SourceUpdate
  }
type Answer = Int

readInput :: FilePath -> IO Source
readInput f = do
  content <- readFile f
  undefined

solve :: Source -> Answer
solve source = undefined

main :: IO ()
main = getFilePath >>= readInput >>= print . solve
