module Main where

import Data.Functor ((<&>))
import Data.List (sort)
import System.Environment (getArgs)

splitBy :: Char -> String -> [String]
splitBy d s = [x | x <- go "" s, not (null x)]
 where
  go :: String -> String -> [String]
  go acc [x] = if x == d then [acc] else [acc <> [x]]
  go acc (x : xs) = if x == d then [acc] <> go "" xs else go (acc <> [x]) xs
  go _ "" = []

getFilePath :: IO FilePath
getFilePath = getArgs <&> head

readInput :: FilePath -> IO [(Int, Int)]
readInput f = do
  content <- readFile f
  let rows = map (splitBy ' ') (splitBy '\n' content)
  return $ map (\l -> (read $ head l, read $ l !! 1)) rows

solve :: [(Int, Int)] -> Int
solve ps =
  let a = sort (map fst ps)
      b = sort (map snd ps)
   in sum $ zipWith (\x y -> abs (x - y)) a b

main :: IO ()
main = do
  source <- getFilePath >>= readInput
  print $ solve source
