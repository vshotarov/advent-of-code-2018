module Main where

import qualified Common
import qualified Data.Vector as V

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let squarePower = sum . map (powerAt parsedInput)
    let answer1 = snd . maximum
                $ map (\c -> (squarePower $ toSquare 3 c, c))
                      [(x,y) | x <- [1..301-3], y <- [1..301-3]]
    let sat = makeSAT parsedInput
    let answer2 = maximum $ map (\d@(x,y,s) -> (sampleSAT sat (x,y) s, d)) allSquares

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Cell = (Int,Int)

parse :: String -> Int
parse = read

toSquare :: Int -> Cell -> [Cell]
toSquare n (x,y) = [(x',y') | x' <- [x..x+(n-1)], y' <- [y..y+(n-1)]]

toSquares :: Cell -> [[Cell]]
toSquares c@(x,y) = map (\s -> toSquare s c) [1..maxSize+1]
    where maxSize = min (300-x) (300-y)

powerAt :: Int -> Cell -> Int
powerAt s (x,y) = (hundrethOnly ((rackId * y + s) * rackId)) - 5
    where rackId = x + 10
          hundrethOnly = (`div` 100) . (`mod` 1000)

cells :: [Cell]
cells = [(x,y) | x <- [1..300], y <- [1..300]]

makeSAT :: Int -> V.Vector Int
makeSAT sn = V.fromList . reverse $ go (1,1) 0 []
    where go (x,y) s out
            | x == 300 && y == 300 = out'
            | x == 300 = go (1,y+1) 0 out'
            | otherwise = go (x+1,y) (s+p) out'
            where p = powerAt sn (x,y)
                  a = if y == 1 then 0 else (out !! 299)
                  out' = (s+p+a):out

cellToI :: Cell -> Int
cellToI (x,y) = (y-1)*300+(x-1)

sampleSAT :: V.Vector Int -> Cell -> Int -> Int
sampleSAT sat (x,y) size = d + a - b - c
    where get (x',y')
            | x' < 1 || y' < 1 = 0
            | otherwise = sat V.! cellToI (x',y')
          a = get (x-1,y-1)
          b = get (x-1+size,y-1)
          c = get (x-1,y+size-1)
          d = get (x-1+size,y+size-1)

allSquares :: [(Int,Int,Int)]
allSquares = concat [[(x,y,s) | s <- ss]
                     | x <- [1..299], y <- [1..299],
                       let ss = [1..(min (300-x) (300-y))]]

