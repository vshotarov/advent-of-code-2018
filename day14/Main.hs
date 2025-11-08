module Main where

import qualified Common
import qualified Data.Sequence as Seq

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let scores = 3 : 7 : buildScores (Seq.fromList [3,7]) 0 1
    let answer1 = read . concatMap show . take 10 $ drop parsedInput scores :: Int
    let inputDigits = toDigits parsedInput
    let answer2 = length $ go scores
            where go (x:xs)
                    | take (length inputDigits) (x:xs) == inputDigits = []
                    | otherwise = x:(go xs)
                  go _ = error "impossible error"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

--        scores    currentA, currentB
type State = (Seq.Seq Int, (Int, Int))

parse :: String -> Int
parse = read

step :: State -> State
step (recipes, (elfA, elfB)) = (recipes', (elfA', elfB'))
    where a = Seq.index recipes elfA
          b = Seq.index recipes elfB
          s = a + b
          d = if s >= 10 then [1, s `mod` 10] else [s]
          recipes' = foldl (\acc x -> acc Seq.|> x) recipes d
          elfA' = (elfA + 1 + a) `mod` (Seq.length recipes')
          elfB' = (elfB + 1 + b) `mod` (Seq.length recipes')

buildScores :: Seq.Seq Int -> Int -> Int -> [Int]
buildScores s a b = score ++ buildScores scores' a' b'
    where as = Seq.index s a
          bs = Seq.index s b
          score = toDigits $ as + bs
          scores' = foldl (\acc x -> acc Seq.|> x) s score
          a' = (a + 1 + as) `mod` (Seq.length scores')
          b' = (b + 1 + bs) `mod` (Seq.length scores')

toDigits :: Int -> [Int]
toDigits = reverse . go
    where go x
            | x < 10 = [x]
            | otherwise = (x `mod` 10):(go $ x `div` 10)

