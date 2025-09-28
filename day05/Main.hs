module Main where

import qualified Common
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ reduction parsedInput
    let answer2 = minimum
                $ map (\c -> length $ (reductionWithStripping c) parsedInput)
                      "abcdefghijklmnopqrstuvwxyz"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> String
parse input = input

alchReduce :: (String -> String) -> String -> String
alchReduce reductionF x = if x == reduced then x else alchReduce reductionF reduced
    where reduced = reductionF x

reduction :: String -> String
reduction = foldr foldOne []
    where foldOne a [] = [a]
          foldOne a (b:bs)
            | canReact a b = bs
            | otherwise = a:b:bs

reductionWithStripping :: Char -> String -> String
reductionWithStripping c = reduction . filter (\cc -> c /= cc && (not $ canReact c cc))

canReact :: Char -> Char -> Bool
canReact a b = abs (ord a - ord b) == 32

