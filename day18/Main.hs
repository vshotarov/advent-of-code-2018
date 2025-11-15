module Main where

import qualified Common
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let size = length parsedInput
        states = iterate (step size) parsedInput
        hash state = trees * lumberyards
            where trees = length $ concatMap (filter (=='|')) state
                  lumberyards = length $ concatMap (filter (=='#')) state
    let answer1 = hash . last $ take 11 states
    let answer2 = hash . last . take stepsAfter . iterate (step size) . head
                $ drop nBeforeRepeat states
            where n = 1000000001
                  go seen (s:tates)
                    | s `elem` seen = (length seen, fromJust . elemIndex s $ reverse seen)
                    | otherwise = go (s:seen) tates
                  (nBeforeRepeat, repeatedId) = go [] states
                  repetitions = (n-nBeforeRepeat) `div` (nBeforeRepeat-repeatedId)
                  stepsAfter = n - (nBeforeRepeat + repetitions * (nBeforeRepeat-repeatedId))

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Grid = [String]
type Cell = (Int,Int)

parse :: String -> Grid
parse input = lines input

step :: Int -> Grid -> Grid
step n grid = [[ next (x,y) | x <- [0..n-1]] | y <- [0..n-1]]
    where trees = length . filter ((=='|') . get n grid)
          lumberyards = length . filter ((=='#') . get n grid)
          next (x,y)
            | grid !! y !! x == '.' =
                if trees ns >= 3 then '|' else '.'
            | grid !! y !! x == '|' =
                if lumberyards ns >= 3 then '#' else '|'
            | otherwise =
                if lumberyards ns >= 1 && trees ns >= 1 then '#' else '.'
            where ns = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1],
                                  (x,y) /= (x',y')]

get :: Int -> Grid -> Cell -> Char
get n grid (x,y)
  | y < 0 || y >= n || x < 0 || x >= n = '.'
  | otherwise = grid !! y !! x

asStr :: Grid -> String
asStr = unlines

