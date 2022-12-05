module Main (main) where

import Data.List (elemIndex, intersect, nub)
import Data.Maybe (fromJust)

findDuplicates :: String -> [Char]
findDuplicates inp = fstHalf `intersect` sndHalf
  where
    midpoint = length inp `div` 2
    fstHalf = take midpoint inp
    sndHalf = drop midpoint inp

priority :: Char -> Int
priority needle = 1 + fromJust (elemIndex needle (['a' .. 'z'] ++ ['A' .. 'Z']))

segmentList :: [a] -> Int -> [[a]]
segmentList [] _ = []
segmentList inp n = take n inp : segmentList (drop n inp) n

main :: IO ()
main = do
  file <- readFile "./input.txt"
  print $ sum $ map (priority . head . nub . foldr intersect (['a' .. 'z'] ++ ['A' .. 'Z'])) $ segmentList (lines file) 3
  -- print $ sum $ map (sum . (map priority . nub . findDuplicates)) (lines file)
