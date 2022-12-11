module Main (main) where

import Data.Functor ((<&>))

data Instruction = Add Int | Noop
  deriving (Show)

type Cycle = Int

-- can be done with a custom read derivation, but not too familiar with that
parseInstruction :: String -> Instruction
parseInstruction inpt = case words inpt of
  ["noop"] -> Noop
  ["addx", i] -> Add (read i)

instructionCycle :: Instruction -> Cycle
instructionCycle Noop = 1
instructionCycle (Add _) = 2

applyInstruction :: [Int] -> Instruction -> [Int]
applyInstruction stack@(x : xs) Noop = x : stack
applyInstruction stack@(x : xs) (Add y) = (x + y) : x : stack

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"


part1 :: IO ()
part1 = do
  instructions <- readFile "./input.txt" <&> map parseInstruction . lines
  let cycles = reverse $ foldl applyInstruction [1] instructions
      strenght = sum [idx * cycles !! (idx - 1) | idx <- [20, 60, 100, 140, 180, 220]]
  print strenght

pixelState :: Int -> [Bool]
pixelState x = [(idx >= x - 1) && (idx <= x + 1) | idx <- take 40 [0..]]

screenRender :: [(Int, Int)] -> [Char]
screenRender [] = []
screenRender ((x, c):xs) = pixel : screenRender xs
  where pixel = if (c >= (x - 1)) && (c <= (x + 1))  then '#' else '.'

part2 :: IO ()
part2 = do
  instructions <- readFile "./input.txt" <&> map parseInstruction . lines
  let cycles = reverse $ foldl applyInstruction [1] instructions
  mapM_ (print . screenRender . zip [0 .. ]) (group 40 cycles)


main :: IO ()
main = part1 >> part2
