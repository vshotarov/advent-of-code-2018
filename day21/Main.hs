module Main where

import qualified Common
import qualified Data.Set as S
import Data.Bits
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    let (answer1,answer2) = solve S.empty parsedInput (0,0,0)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (Int,Int)
parse input = (read . (!! 1) . splitOn " " $ asLines !! 8,
               read . (!! 2) . splitOn " " $ asLines !! 7)
    where asLines = lines input

solve :: S.Set Int -> (Int,Int) -> (Int,Int,Int) -> (Int,Int)
solve seen (d,f) (a1,p,a2) = do
    let c = f .&. 255
        d' = (65899 * ((d + c) .&. 16777215)) .&. 16777215
        a1' = if (256 > f && S.null seen) then d' else a1
        a2' = if (256 > f && (d' `S.member` seen)) then p else a2
        seen' = if 256 > f then S.insert d' seen else seen
        p' = if 256 > f then d' else p
        f' = if 256 > f then (d' .|. 65536) else (f `div` 256)
        d'' = if 256 > f then 5557974 else d'

    if a1'/=0 && p' /= 0 && a2'/=0
    then (a1',a2')
    else solve seen' (d'',f') (a1',p',a2')

