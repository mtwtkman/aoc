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

type Source = [[Int]]
type Answer = Int

readInput :: FilePath -> IO Source
readInput f = do
  content <- readFile f
  let rows = splitBy '\n' content
  return $ map (map read . splitBy ' ') rows

rule2 :: Int -> Int -> Bool
rule2 a b = let x = abs (a - b) in x >= 1 && x <= 3

solve :: Source -> Answer
solve source = sum $ map calc source
 where
  calc :: [Int] -> Answer
  calc xs = go (compare (head xs) (xs !! 1)) xs

  go :: Ordering -> [Int] -> Answer
  go _ [] = error "redundant"
  go _ [_] = error "redundant"
  go o [a, b] = if compare a b == o && rule2 a b then 1 else 0
  go o (a : b : rest) = if compare a b == o && rule2 a b then go o (b : rest) else 0

makePairs :: [Int] -> [(Int, Int)]
makePairs [x, y] = [(x,y)]
makePairs (x:y:rest) = (x,y) : makePairs (y : rest)
makePairs [_] = []
makePairs [] = []

solve2 :: Source -> Answer
solve2 source = sum (map (calc True . makePairs) source)
 where
  calc :: Bool -> [(Int, Int)] -> Answer
  calc a xs = if go a xs (uncurry compare (head xs)) then 1 else 0

  go :: Bool -> [(Int, Int)] -> Ordering -> Bool
  go False _ _ = False
  go True [] _ = True
  go True ((x, y) : xs) o = (compare x y == o && rule2 x y) && go True xs o

main :: IO ()
main = getFilePath >>= readInput >>= print . solve2
