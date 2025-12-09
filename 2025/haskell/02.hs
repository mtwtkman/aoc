module Main where

import Data.List (nub, splitAt)

makePair :: String -> (String, String)
makePair p = (takeWhile (/= '-') p, tail $ dropWhile (/= '-') p)

splitComma :: String -> [String]
splitComma s = go s "" []
 where
  go :: String -> String -> [String] -> [String]
  go [] [] result = result
  go [] acc result = result <> [acc]
  go (x : xs) acc result
    | x == ',' = go xs "" (result <> [acc])
    | otherwise = go xs (acc <> [x]) result

parse :: String -> [(String, String)]
parse s = map makePair $ splitComma s

readInput :: String -> IO [(String, String)]
readInput f = do
  input <- readFile f
  return $ map makePair (splitComma input)

solvePart1 :: [(String, String)] -> Int
solvePart1 = foldl (\acc (f, l) -> acc + sum (map (search . show) [(read f :: Int) .. (read l :: Int)])) 0
 where
  search :: String -> Int
  search s =
    let size = length s
     in if odd size
          then 0
          else
            let half = size `div` 2 in if take half s == drop half s then read s else 0

groupByLength :: String -> Int -> [String]
groupByLength s n
  | length s `mod` n /= 0 = []
  | otherwise = go [] s n
 where
  go :: [String] -> String -> Int -> [String]
  go acc "" _ = acc
  go acc st ln =
    let (x, xs) = splitAt ln st
     in go (acc <> [x]) xs n

search :: String -> Int
search s =
  let keys = [take l s | l <- [1 .. (length s `div` 2)]]
   in if not $ any (\k -> nub (groupByLength (drop (length k) s) (length k)) == [k]) keys
        then 0
        else read s

solvePart2 :: [(String, String)] -> Int
solvePart2 = foldl (\acc (f, l) -> acc + sum (map (search . show) [(read f :: Int) .. (read l :: Int)])) 0

example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862"

runExamplePart1 = print $ solvePart1 $ parse example
runPart1 = readInput "02.input" >>= print . solvePart1

example2 = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
runExamplePart2 = print $ solvePart2 $ parse example2

runPart2 = readInput "02.input" >>= print . solvePart2

main :: IO ()
main = runPart2
