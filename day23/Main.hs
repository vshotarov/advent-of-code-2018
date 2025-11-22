module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn, sort)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ inRadius
            where maxRadius = maximum $ map snd parsedInput
                  strongest = filter ((==maxRadius) . snd) parsedInput
                  getInRadius (p,r) = filter (\(op,_) -> manhattan p op <= r) parsedInput
                  numInRadius = map (\nb -> (nb,getInRadius nb)) strongest
                  inRadius = fst . last $ sortOn (length . snd) numInRadius
    let answer2 = search parsedInput startAABB
            where (smallN,bigN) = (-1000000000, 10000000000)
                  minMax m ((x,y,z),_) (mx,my,mz) = (m mx x, m my y, m mz z)
                  low = foldr (minMax min) (bigN,bigN,bigN) parsedInput
                  high = foldr (minMax max) (smallN,smallN,smallN) parsedInput
                  startAABB = (low,high)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int,Int)
type Nanobot = (Vec,Int)
type AABB = (Vec,Vec)

parse :: String -> [Nanobot]
parse input = map parseOne $ lines input
    where parseOne l = let (posPart,rPart) = Common.splitOnceOn ", " l
                           [x,y,z] = map read . splitOn "," . drop 5 $ init posPart
                           r = read $ drop 2 rPart
                        in ((x,y,z),r)

search :: [Nanobot] -> AABB -> Int
search bots aabb = go [(-numBotsInAABB bots aabb, pointDistanceToAABB (0,0,0) aabb, size aabb, aabb)]
    where toNode aabb' = (-numBotsInAABB bots aabb',
                           pointDistanceToAABB (0,0,0) aabb',
                           size aabb',
                           aabb')
          go [] = error "could not find solution"
          go ((_,d,s,aabb'):queue)
            | s == 0 = d
            | otherwise = let subs = map toNode $ subdivideAABB aabb'
                           in go . sort $ subs ++ queue

numBotsInAABB :: [Nanobot] -> AABB -> Int
numBotsInAABB bots aabb = length $ filter (\(p,r) -> pointDistanceToAABB p aabb <= r) bots

subdivideAABB :: AABB -> [AABB]
subdivideAABB ((x1,y1,z1),(x2,y2,z2)) = S.toList . S.fromList $ [a,b,c,d,e,f,g,h]
    where (dx,dy,dz) = (x2-x1,y2-y1,z2-z1)
          (mx,my,mz) = (x1 + dx `div` 2, y1 + dy `div` 2, z1 + dz `div` 2)
          a = ((x1,y1,z1),(mx,my,mz))
          b = ((mx+1,y1,z1),(x2,my,mz))
          c = ((x1,my+1,z1),(mx,y2,mz))
          d = ((mx+1,my+1,z1),(x2,y2,mz))
          e = ((x1,y1,mz+1),(mx,my,z2))
          f = ((mx+1,y1,mz+1),(x2,my,z2))
          g = ((x1,my+1,mz+1),(mx,y2,z2))
          h = ((mx+1,my+1,mz+1),(x2,y2,z2))

pointDistanceToAABB :: Vec -> AABB -> Int
pointDistanceToAABB p@(x,y,z) ((x1,y1,z1),(x2,y2,z2)) = manhattan clamped p
    where clamped = (min (max x1 x) x2, min (max y1 y) y2, min (max z1 z) z2)

center :: AABB -> Vec
center ((x1,y1,z1),(x2,y2,z2)) = ((x1+x2) `div` 2, (y1+y2) `div` 2, (z1+z2) `div` 2)

size :: AABB -> Int
size = uncurry manhattan

isInRadius :: Vec -> Nanobot -> Bool
isInRadius p' (nbp,r) = manhattan p' nbp <= r

manhattan :: Vec -> Vec -> Int
manhattan (a,b,c) (x,y,z) = abs (x-a) + abs (y-b) + abs (z-c)

