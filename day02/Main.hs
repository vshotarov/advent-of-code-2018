module Main where

import qualified Common
import qualified Data.Map as M
import Data.Maybe (isJust)

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length twos * length threes
            where toCharCount :: String -> M.Map Char Int
                  toCharCount = foldr (\c -> M.insertWith (+) c 1) M.empty
                  charCounts = map toCharCount parsedInput
                  twos = filter ((2 `elem`) . M.elems) charCounts
                  threes = filter ((3 `elem`) . M.elems) charCounts

    let answer2 = Common.firstWhere (isJust)
                      [allButOneMatch a b | a <- parsedInput, b <- parsedInput]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [String]
parse input = lines input

allButOneMatch :: String -> String -> Maybe String
allButOneMatch a b | length a /= length b = Nothing
allButOneMatch [] _ = Just []
allButOneMatch a b = if (length res == length a - 1) then Just res else Nothing
    where go _ [] _ = []
          go diff (x:xs) (y:ys)
            | x == y    = x:(go diff xs ys)
            | diff      = []
            | otherwise = go True xs ys
          res = go False a b

