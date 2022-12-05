module Main (main) where

import Control.Monad
import Data.Char (isSpace)
import Data.List (transpose)

type Crate = Char

type Stack = [Crate]

type Stacks = [Stack]

type CrateMover = Stacks -> Move -> Stacks

-- Amount, From, To
data Move = Move Int Int Int

parseCrates :: String -> [Crate]
parseCrates [] = []
parseCrates (' ' : '1' : _) = [] -- cheeky way to ignore the indexes in the input
parseCrates xs = crate : parseCrates (drop 4 xs)
  where
    [_, crate, _] = take 3 xs

parseMovement :: String -> Move
parseMovement input = Move (read amount) (read from) (read to)
  where
    [_, amount, _, from, _, to] = words input

crateMover :: ([Crate] -> [Crate]) -> CrateMover
crateMover moveOp stacks (Move amount from to) = newStacks
  where
    (heads, sourceStack : tails) = splitAt (from - 1) stacks
    toTake = take amount sourceStack
    intermediate = heads ++ drop amount sourceStack : tails
    (hs, targetStack : ts) = splitAt (to - 1) intermediate
    newStacks = hs ++ (moveOp toTake ++ targetStack) : ts

crateMover9000 = crateMover reverse

crateMover9001 = crateMover id

solution :: CrateMover -> String -> IO Stacks
solution mover input = return $ foldl mover crateStacks movementInput
  where
    cratesInputRaw = takeWhile (not . null) (lines input)
    -- dropWhile includes the failing character at the head of the list
    (_ : movementInputRaw) = dropWhile (not . null) (lines input)
    movementInput = map parseMovement movementInputRaw
    crateStacks = map (dropWhile isSpace) $ transpose $ map parseCrates cratesInputRaw

part1 = solution crateMover9000

part2 = solution crateMover9001

main :: IO ()
main = readFile "./input.txt" >>= part1 >>= print . map head
