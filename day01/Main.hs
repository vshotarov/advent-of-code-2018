module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum parsedInput
    let answer2 = findFirstDuplicateFrequency parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse = map (read . dropWhile (== '+')) . lines

findFirstDuplicateFrequency :: [Int] -> Int
findFirstDuplicateFrequency []     = error "CALIBRATION FAILED: no frequency changes"
findFirstDuplicateFrequency (c:cs) = go (S.singleton c) c cs
    where go states b [] = go states b (c:cs)
          go states b (x:xs)
            | (x+b) `S.member` states = x+b
            | otherwise = go (S.insert (x+b) states) (x+b) xs

