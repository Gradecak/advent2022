module Main (main) where
import Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

partitionSegments :: [String] -> [[String]]
partitionSegments = partitionSegments' []
  where partitionSegments' [] group = [group]
        partitionSegments' (x:xs) group
          | (not . null) x =  partitionSegments' xs $ x : group
          | otherwise = group : partitionSegments' xs []

totals :: [[String]] -> [Int]
totals = map $ sum . map read

main :: IO ()
main = do
  l <- readLines "./input.txt"
  print $ sum $ take 3 $ sortBy (flip compare) $ totals (partitionSegments l)
