module Main where

import qualified Common
import Data.Tuple (swap)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (rangeX,rangeY) = ((minimum xs, maximum xs),(minimum ys, maximum ys))
            where (xs,ys) = unzip $ S.toList parsedInput
        filled@(water,_) = fill rangeX rangeY parsedInput (500,0)
    let answer1 = S.size $ (uncurry S.union) filled
    let answer2 = S.size water

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Cell = (Int,Int)

parse :: String -> S.Set Cell
parse input = S.fromList . concatMap (expand . parseLine) $ lines input
    where parseVal [] = error "invalid input"
          parseVal v
            | '.' `elem` v = let (left,right) = Common.splitOnceOn "." v
                              in (read left, read $ filter (/='.') right)
            | otherwise = (read v, read v)
          parseLine [] = error "invalid input"
          parseLine l@(x:_) = (if x == 'x' then id else swap)
                            . Common.mapTuple (parseVal . snd . Common.splitOnceOn "=")
                            $ Common.splitOnceOn ", " l
          expand ((x1,x2),(y1,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

fill :: (Int,Int) -> (Int,Int) -> S.Set Cell -> Cell -> (S.Set Cell, S.Set Cell)
fill (minX,maxX) (minY,maxY) clay spring = Common.mapTuple removeOutOfBounds
                                         $ go S.empty S.empty [spring]
  where removeOutOfBounds = S.filter (\(_,y) -> y >= minY && y <= maxY)
        go water wet [] = (water,wet)
        go water wet (c@(x,y):queue)
          | y > maxY || isHard c =
              -- skip
              go water wet queue
          | isHard (x,y+1) =
              -- fill level OR spill to the sides
              if floored
              then go (S.union water thisLevel) wet' $ wetPrevLevel ++ queue
              else go water wet' . filter (not . isWet) $ ((x-1,y):(x+1,y):queue)
          | otherwise =
              -- flow down
              go water wet' ((x,y+1):queue)
          where isWet = (`S.member` wet)
                isWater = (`S.member` water)
                isClay = (`S.member` clay)
                isHard p = (isWater p) || (isClay p)

                boundary dx (x',y')
                  | isHard (x',y') || x' < minX || x' > maxX = x'-dx
                  | otherwise = boundary dx (x'+dx,y')

                (left,right) = (boundary (-1) c, boundary 1 c)
                thisLevel = S.fromList [(x',y) | x' <- [left..right]]
                nextLevel = [(x',y+1) | x' <- [left..right]]
                wetPrevLevel = [(x',y-1) | x' <- [left..right], isWet (x',y-1)]
                floored = all isHard nextLevel

                wet' = S.insert (x,y) wet

asStr :: (Int,Int) -> (Int,Int) -> S.Set Cell -> (S.Set Cell,S.Set Cell) -> String
asStr (minX,maxX) (minY,maxY) clay (water,wet) =
    concat [[toChar (x,y) | x <- [minX..maxX]] ++ "\n" | y <- [minY..maxY]]
    where toChar c
            | c `S.member` water = '~'
            | c `S.member` wet = '|'
            | c `S.member` clay = '#'
            | otherwise = '.'

