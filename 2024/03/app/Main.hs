module Main where

import Data.Char (isDigit)
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

type Source = String
type Answer = Int

readInput :: FilePath -> IO Source
readInput = readFile

isNumberString :: String -> Bool
isNumberString = foldl (\acc c -> isDigit c && acc) True

findOperands :: String -> [(Int, Int)]
findOperands source =
  let (r, _, _) =
        foldl
          ( \(res, mulAcc, opAcc) c -> case (mulAcc, opAcc, c) of
              ("", _, 'm') -> (res, "m", initialOpAcc)
              ("m", _, 'u') -> (res, "mu", initialOpAcc)
              ("mu", _, 'l') -> (res, "mul", initialOpAcc)
              ("mul", _, '(') -> (res, "mul(", initialOpAcc)
              ("mul(", (Nothing, Nothing), _) -> if isDigit c then (res, mulAcc, (Just [c], Nothing)) else (res, "", initialOpAcc)
              ("mul(", (Just x, Nothing), ',') -> (res, mulAcc, (Just x, Just ""))
              ("mul(", (Just x, Nothing), _) -> if isDigit c then (res, mulAcc, (Just (x <> [c]), Nothing)) else (res, "", initialOpAcc)
              ("mul(", (Just x, Just y), ')') -> if length x <= 3 && length y <= 3 then ((read x, read y) : res, "", initialOpAcc) else (res, "", initialOpAcc)
              ("mul(", (Just x, Just y), _) -> if isDigit c then (res, mulAcc, (Just x, Just (y <> [c]))) else (res, "", initialOpAcc)
              (_, (Nothing, Just _), _) -> (res, "", initialOpAcc)
              (_, _, _) -> (res, "", (Nothing, Nothing))
          )
          initialAcc
          source
   in r
 where
  initialOpAcc :: (Maybe String, Maybe String)
  initialOpAcc = (Nothing, Nothing)

  initialAcc :: ([(Int, Int)], String, (Maybe String, Maybe String))
  initialAcc = ([], "", initialOpAcc)

solve :: Source -> Answer
solve source = sum (map (uncurry (*)) $ findOperands source)

main :: IO ()
main = getFilePath >>= readInput >>= print . solve
