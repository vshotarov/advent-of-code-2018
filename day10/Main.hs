module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    do
        putStrLn "Beginning interactive finding of solution"
        loop 0 $ iterate (map (step 1)) parsedInput

loop :: Int -> [[State]] -> IO ()
loop _ [] = error "no states"
loop i (s:tates)
    | not (isInReasonableRange s) = loop (i+1) tates
    | otherwise = do
        putStrLn (draw s)
        putStrLn "Is this the solution? (y/n)"
        input <- getLine
        if input == "y"
           then putStrLn $ "Nice! The answer to part 2 is " ++ show (i :: Int)
           else loop (i+1) tates

type Vec = (Int,Int)
type State = (Vec,Vec)

parse :: String -> [State]
parse = map parseOne . lines
    where parseVec = Common.mapTuple read . Common.splitOnceOn ", "
                   . init . tail -- remove < and >
          parseOne l = case splitOn " v" l of
                         [p,v] -> Common.mapTuple (parseVec . dropWhile (/='<')) (p,v)
                         _     -> error "unrecognised line format"

step :: Int -> State -> State
step n ((px,py), v@(vx,vy)) = ((px+n*vx, py+n*vy), v)

draw :: [State] -> String
draw states = unlines [[if (x,y) `S.member` positionsS
                            then '#' else '.'
                       | x <- [minX..maxX]]
                      | y <- [minY..maxY]]
    where ((minX,maxX),(minY,maxY)) = getRange states
          positionsS = S.fromList $ map fst states

getRange :: [State] -> ((Int,Int),(Int,Int))
getRange states = ((minX,maxX),(minY,maxY))
    where positions = map fst states
          (xs,ys) = unzip positions
          (minX,maxX) = (minimum xs, maximum xs)
          (minY,maxY) = (minimum ys, maximum ys)

isInReasonableRange :: [State] -> Bool
isInReasonableRange states = dx < 100 && dy < 100
    where ((minX,maxX),(minY,maxY)) = getRange states
          (dx,dy) = (maxX-minX,maxY-minY)

