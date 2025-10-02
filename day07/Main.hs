module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( sort, partition)
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let letters = S.toList . S.fromList $ concatMap (\(a,b) -> [a, head b]) parsedInput
        dependencies = M.fromListWith (++) parsedInput
    let answer1 = concatMap fst $ solve 1 0 (const 0) letters S.empty dependencies [] 0
    let answer2 = snd . last $ solve 5 60 (\l -> ord l - 64) letters S.empty dependencies [] 0

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [(Char, String)]
parse = map f . lines
    where f l = let ws = words l in (head $ ws !! 7, ws !! 1)

solve :: Int -- num elves
      -> Int -- extra time
      -> (Char -> Int) -- time to next step function
      -> String -> S.Set Char -- set of solved letters
      -> M.Map Char String -- dependencies
      -> [(Char,Int)] -- list of current workers
      -> Int -- curent time step
      -> [(String,Int)]
solve numElves extraTime timeF letters solved deps workers t
    | null workers' = [output]
    | otherwise = output : solve numElves extraTime timeF letters solved' deps' workers' nextT
    where (finished,inProgress) = partition ((<=t) . snd) workers

          (solved', deps') = foldr foldF (solved,deps) finished
              where foldF (l,_) (s,d) = (S.insert l s, M.map (filter (/=l)) d)

          isAvailable l =
              (null (M.findWithDefault [] l deps'))
           && (not (l `S.member` solved'))
           && (not (any ((==l) . fst) inProgress))

          new = take (numElves - length inProgress)
                     [(l, t + (timeF l) + extraTime) | l <- letters, isAvailable l]

          workers' = inProgress ++ new
          nextT = minimum (map snd workers')
          output = (sort (map fst finished),t)

