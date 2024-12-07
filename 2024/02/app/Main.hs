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

type Source = [(Int, Int, Int, Int, Int)]
type Answer = Int

readInput :: FilePath -> IO Source
readInput f = do
  content <- readFile f
  let rows = splitBy '\n' content
  return $ map ((\l -> (read $ head l, read $ l !! 1, read $ l !! 2, read $ l !! 3, read $ l !! 4)) . splitBy ' ') rows

rule2 :: Int -> Int -> Bool
rule2 a b = let x = abs (a - b) in x >= 1 && x <= 3

tplToList :: (Int, Int, Int, Int, Int) -> [Int]
tplToList (a, b, c, d, e) = [a, b, c, d, e]

solve :: Source -> Answer
solve source = sum $ map (calc . tplToList) source
 where
  calc :: [Int] -> Answer
  calc xs = go (compare (head xs) (xs !! 1)) xs

  go :: Ordering -> [Int] -> Answer
  go _ [] = error "redundant"
  go _ [_] = error "redundant"
  go o [a, b] = if compare a b == o && rule2 a b then 1 else 0
  go o (a : b : rest) = if compare a b == o && rule2 a b then go o (b : rest) else 0

makePairs :: (Int, Int, Int, Int, Int) -> [(Int, Int)]
makePairs (a, b, c, d, e) = [(a, b), (b, c), (c, d), (d, e)]

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
