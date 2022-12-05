module Main (main) where

import Data.Char
import Data.List

splitOn :: Char -> String -> [String]
splitOn _ [] = [[]]
splitOn delimiter (x : xs)
  | x /= delimiter = (x : group) : rest
  | otherwise = [] : splitOn delimiter xs
  where
    (group : rest) = splitOn delimiter xs

parseRange :: String -> [Int]
parseRange input = [rangeStart .. rangeEnd]
  where
    rangeStart = read $ takeWhile isNumber input
    rangeEnd = read $ dropWhile isPunctuation $ dropWhile isNumber input

hasAnyOverlap :: [Int] -> [Int] -> Bool
hasAnyOverlap x y = not $ null (x `intersect` y)

hasCompleteOverlap :: [Int] -> [Int] -> Bool
hasCompleteOverlap x y = length (x `intersect` y) == min (length x) (length y)

toTuple :: [[Int]] -> ([Int], [Int])
toTuple [x, y] = (x, y)
toTuple _ = error "Too many values to unpack"

part2 :: IO ()
part2 = do
  l <- readFile "./input.txt"
  print . length . filter (== True) $ map overlapCalculator (lines l)
  where
    overlapCalculator = uncurry hasAnyOverlap . toTuple . map parseRange . splitOn ','

part1 :: IO ()
part1 = do
  l <- readFile "./input.txt"
  print . length . filter (== True) $ map overlapCalculator (lines l)
  where
    overlapCalculator = uncurry hasCompleteOverlap . toTuple . map parseRange . splitOn ','

main :: IO ()
main = part2
