module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.List (sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    let sleepData = M.fromListWith (++) $ logsToSleepPatterns parsedInput
        totals = M.map length sleepData
        sleepiestGuard = snd . maximum . map swap $ M.toList totals
        sleepiestMinutes = M.map (getSleepiestMinute . map (`mod` 60)) sleepData

    -- Solve
    let answer1 = sleepiestGuard * (snd $ sleepiestMinutes M.! sleepiestGuard)
    let answer2 = guard * minute
            where ((_, minute), guard) = maximum . map swap $ M.toList sleepiestMinutes

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Timestamp = Int
type Log = (Timestamp, String)
type Guard = (Int, [Timestamp])

monthDays :: [Int]
monthDays = [31,28,31,30,31,30,31,31,30,31,30,31]

parse :: String -> [Log]
parse input = sort . map (uncurry parseLog . Common.splitOnceOn "] " . tail) $ lines input
    where parseLog datetime action = (toTimestamp datetime, action)
          toTimestamp (_:_:_:_:_:mm1:mm2:_:d1:d2:_:h1:h2:_:m1:m2:[]) = 
              (sum (take (read [mm1,mm2]) monthDays) + read [d1,d2]) * 24 * 60
              + read [h1,h2] * 60 + read [m1,m2]
          toTimestamp t = error ("bad timestamp format: " ++ t)

logsToSleepPatterns :: [Log] -> [Guard]
logsToSleepPatterns = tail . go False 0 [] 0
    where go _ _ buffer guard [] = [(guard,buffer)]
          go _ _ buffer guard ((t,('G':ction)):logs) =
              (guard,buffer):(go False t [] guard' logs)
              where guard' = read. tail $ (words ction) !! 1
          go False _ buffer guard ((t,('f':_)):logs) =
              go True t buffer guard logs
          go True prevTime buffer guard ((t,('w':_)):logs) =
              go False t (buffer ++ [prevTime..t-1]) guard logs
          go _ _ buffer guard ((t,_):logs) = go False t buffer guard logs

timesToMinutes :: [Timestamp] -> [Timestamp]
timesToMinutes times = concatMap rangeToMinutes . chunksOf 2 $ tail times
    where rangeToMinutes [a,b] = [a..b-1]
          rangeToMinutes _   = []

frequencies :: Ord b => (c -> c -> c) -> (a -> b) -> (a -> c) -> [a] -> M.Map b c
frequencies sumOp keyF freqF = foldr (\x -> M.insertWith sumOp (keyF x) (freqF x)) M.empty

getSleepiestMinute :: [Int] -> (Int,Int)
getSleepiestMinute [] = (0,0)
getSleepiestMinute ms = maximum . map swap . M.toList $ frequencies (+) id (\_ -> 1) ms

