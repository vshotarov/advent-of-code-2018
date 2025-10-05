module Main where

import qualified Common
import CircularList
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(nPlayers,lastMarbleScore) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let step (i,c,s)
            | i `mod` 23 == 0 = (i+1, c'', s')
            | otherwise = (i+1,shiftRight . shiftRight $ insert i c,s)
            where c' = last . take 10 $ iterate shiftLeft c
                  c'' = shiftRight . shiftRight $ dropFocus c'
                  s' = M.insertWith (+) (1 + i `mod` (M.size s)) (i+(focus c')) s
        zeros n = M.fromList [(i,0) | i <- [1..n]]
        startState :: CircularList Int
        startState = CircularList [] 0 []
        solve n l = maximum . M.elems . (\(_,_,s) -> s) . last . take l
                  $ iterate step (1, startState, zeros n)
    let answer1 = solve nPlayers lastMarbleScore
    let answer2 = solve nPlayers (lastMarbleScore*100)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (Int,Int)
parse input = go $ words input
    where go [p,_,_,_,_,_,m,_] = Common.mapTuple read (p, m)
          go _ = error "bad input"

