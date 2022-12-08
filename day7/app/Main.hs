module Main (main) where

import Data.Bifunctor (first)

data FileTree
  = File String Int
  | Dir String Int [FileTree]
  deriving (Show)

dirSize :: [FileTree] -> Int
dirSize [] = 0
dirSize ((File _ size) : xs) = size + dirSize xs
dirSize ((Dir _ size _) : xs) = size + dirSize xs

parseDir' :: [String] -> ([FileTree], [String])
parseDir' [] = ([], [])
parseDir' (x : xs) = case words x of
  ["$", "cd", ".."] -> ([], xs) -- done parsing this subdirectory
  ["$", "cd", dirName] -> first (Dir dirName (dirSize downstream) downstream :) (parseDir' remainder)
  ["$", _] -> skip
  ["dir", _] -> skip
  [fileSize, fileName] -> (File fileName (read fileSize) : downstream, remainder)
  _ -> skip
  where
    skip@(downstream, remainder) = parseDir' xs

parseTree :: [String] -> FileTree
parseTree i = case parseDir' i of
  (tree, []) -> head tree
  (_, leftover) -> error $ "Leftover stuff during parsing: " ++ show leftover

findDirsOfSize :: (Int -> Bool) -> FileTree -> [(String, Int)]
findDirsOfSize predicate = findDirsOfSize'
  where
    findDirsOfSize' :: FileTree -> [(String, Int)]
    findDirsOfSize' (File _ _) = []
    findDirsOfSize' (Dir _ _ []) = []
    findDirsOfSize' (Dir n s subdirs) =
      if predicate s
        then (n, s) : concatMap findDirsOfSize' subdirs
        else concatMap findDirsOfSize' subdirs


part1 :: IO ()
part1 = readFile "./input.txt" >>= print . sum . map snd . findDirsOfSize ( <= 100000) . parseTree . lines

part2 :: IO ()
part2 = do
  file <- readFile "./input.txt"
  let totalSpace = 70000000
      tree@(Dir _ usedSpace _) = parseTree (lines file)
      availableSpace = totalSpace - usedSpace
      neededSpace = 30000000 - availableSpace
      deleteCandidates = findDirsOfSize (>= neededSpace) tree
  print $ minimum (map snd deleteCandidates)
  return ()

main :: IO ()
main = part1 >> part2
