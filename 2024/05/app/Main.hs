module Main where

import Data.Functor ((<&>))
import qualified Data.Map as M
import System.Environment (getArgs)

splitBy :: Char -> String -> [String]
splitBy d = go ""
 where
  go :: String -> String -> [String]
  go acc [x] = if x == d then [acc] else [acc <> [x]]
  go acc (x : xs) = if x == d then [acc] <> go "" xs else go (acc <> [x]) xs
  go _ "" = []

makeSource :: String -> Source
makeSource = go ""
 where
  go :: String -> String -> Source
  go acc (x : '\n' : '\n' : rest) = Source (parseUpdate rest) (parseViolation (acc <> [x]))
  go acc (x : rest) = go (acc <> [x]) rest
  go _ [] = error "somthing wrong"

parseUpdate :: String -> SourceUpdate
parseUpdate s = map (map read . splitBy ',') (splitBy '\n' s)

parseViolation :: String -> M.Map Int [Int]
parseViolation s = go M.empty (splitBy '\n' s)
 where
  go :: M.Map Int [Int] -> [String] -> M.Map Int [Int]
  go acc [] = acc
  go acc (row : rest) =
    let pair = splitBy '|' row
        a = read $ pair !! 1 :: Int
        b = read $ head pair :: Int
        m = M.lookup a acc
     in go
          ( M.insert a (maybe [b] (<> [b]) m) acc
          )
          rest

getFilePath :: IO FilePath
getFilePath = getArgs <&> head

type SourceOrderRule = M.Map Int [Int]
type SourceUpdate = [[Int]]
type Violation = M.Map Int [Int]

data Source = Source
  { update :: SourceUpdate
  , violation :: Violation
  }
  deriving (Show)
type Answer = Int

readInput :: FilePath -> IO Source
readInput f = do
  content <- readFile f
  return $ makeSource content

isValid :: Violation -> Int -> Int -> Bool
isValid v y x = maybe True (notElem x) (M.lookup y v)

allValid :: Violation -> [Int] -> Bool
allValid v (x : y : rest) = isValid v x y && allValid v (y : rest)
allValid _ [_] = True
allValid _ [] = True

mdl :: [Int] -> Int
mdl xs = xs !! (length xs `div` 2)

solve :: Source -> Answer
solve (Source{update = u, violation = v}) = foldr (\row acc -> if allValid v row then acc + mdl row else acc) 0 u

main :: IO ()
main = getFilePath >>= readInput >>= print . solve
