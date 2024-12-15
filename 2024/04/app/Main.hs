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

type Source = [String]
type Answer = Int

type X = Int
type Y = Int
type Pos = (X, Y)

pickChar :: Source -> Pos -> Char
pickChar s (x, y)
  | x < 0 || y < 0 = error ""
  | otherwise = (s !! y) !! x

type Direction = (Pos, Pos, Pos, Pos)
type DirectionFunc = Pos -> Direction

directionUp :: DirectionFunc
directionUp (x, y) = ((x, y), (x, y + 1), (x, y + 2), (x, y + 3))

directionDown :: DirectionFunc
directionDown (x, y) = ((x, y), (x, y - 1), (x, y - 2), (x, y - 3))

directionRight :: DirectionFunc
directionRight (x, y) = ((x, y), (x + 1, y), (x + 2, y), (x + 3, y))

directionLeft :: DirectionFunc
directionLeft (x, y) = ((x, y), (x - 1, y), (x - 2, y), (x - 3, y))

directionUpRight :: DirectionFunc
directionUpRight (x, y) = ((x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3))

directionUpLeft :: DirectionFunc
directionUpLeft (x, y) = ((x, y), (x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3))

directionDownRight :: DirectionFunc
directionDownRight (x, y) = ((x, y), (x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3))

directionDownLeft :: DirectionFunc
directionDownLeft (x, y) = ((x, y), (x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3))

type Row = Int
type Col = Int

canMoveToDown :: Row -> Y -> Bool
canMoveToDown r y = r - y >= 3

canMoveToUp :: Y -> Bool
canMoveToUp y = y >= 3

canMoveToRight :: Col -> X -> Bool
canMoveToRight c x = c - x >= 3

canMoveToLeft :: X -> Bool
canMoveToLeft x = x >= 3

pickCandidate :: Col -> Row -> Pos -> [Direction]
pickCandidate col row pos@(x, y) =
  let u = canMoveToDown row y
      r = canMoveToRight col x
      d = canMoveToUp y
      l = canMoveToLeft x
   in []
        <> ( [directionUp pos | u]
           )
        <> ( [directionRight pos | r]
           )
        <> ( [directionDown pos | d]
           )
        <> ( [directionLeft pos | l]
           )
        <> ( [directionUpRight pos | u && r]
           )
        <> ( [directionDownRight pos | d && r]
           )
        <> ( [directionUpLeft pos | u && l]
           )
        <> ( [directionDownLeft pos | d && l]
           )

readInput :: FilePath -> IO Source
readInput f = do
  content <- readFile f
  return $ splitBy '\n' content

findX :: Source -> [Pos]
findX source = do
  r <- [0 .. (length source - 1)]
  c <- [0 .. (length (head source) - 1)]
  if pickChar source (c, r) == 'X'
    then
      return (c, r)
    else mempty

makeStr :: Source -> Direction -> String
makeStr src (a, b, c, d) = [pickChar src a, pickChar src b, pickChar src c, pickChar src d]

solve :: Source -> Answer
solve source =
  let row = length source - 1
      col = length (head source) - 1
      xs = findX source
      ds = map (pickCandidate col row) xs
   in foldr
        (\d acc -> acc + sum [1 | (x, m, a, s) <- d, [pickChar source x, pickChar source m, pickChar source a, pickChar source s] == "XMAS"])
        0
        ds

main :: IO ()
main = getFilePath >>= readInput >>= print . solve
