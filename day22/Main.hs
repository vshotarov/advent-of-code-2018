module Main where

import qualified Common
import qualified Data.Map.Strict as M
import Data.Sequence as Seq

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(depth, (tx,ty)) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let getType' (x,y) g = if (x,y) `elem` [(0,0),(tx,ty)]
                              then (g,0) else getType depth (x,y) g
    let (geoids,answer1) = foldr (\x (a,b) -> let (a',t) = getType' x a
                                               in (a',b+t))
                                 (M.empty,0) [(x,y) | x <- [0..tx], y <- [0..ty]]
    let answer2 = solve2 (getType depth) geoids (tx,ty)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Cell = (Int,Int)
type Grid = M.Map Cell Int

parse :: String -> (Int,Cell)
parse input = (depth', target')
    where (depth,target) = case lines input of
                             [a,b] -> (a,b)
                             _ -> error "invalid input"
          depth' = read . snd $ Common.splitOnceOn ": " depth
          target' = Common.mapTuple read . Common.splitOnceOn ","
                  . snd $ Common.splitOnceOn ": " target

gidToErosion :: Int -> Int -> Int
gidToErosion depth gid = (gid + depth) `mod` 20183

getGID :: Int -> Cell -> Grid -> (Grid,Int)
getGID depth (x,y) acc
  | (x,y) `M.member` acc = (acc, acc M.! (x,y))
  | x == 0 = (M.insert (x,y) (y*48271) acc, y*48271)
  | y == 0 = (M.insert (x,y) (x*16807) acc, x*16807)
  | otherwise = (M.insert (x,y) c acc'', c)
  where (acc',a) = getGID depth (x-1,y) acc
        (acc'',b) = getGID depth (x,y-1) acc'
        c = gidToErosion depth a * gidToErosion depth b

getType :: Int -> Cell -> Grid -> (Grid,Int)
getType depth (x,y) acc
  | x < 0 || y < 0 = (acc,0)
  | otherwise = (acc',c)
  where (acc',gid) = getGID depth (x,y) acc
        c = (gidToErosion depth gid) `mod` 3

solve2 :: (Cell -> Grid -> (Grid,Int)) -> Grid -> Cell -> Int
solve2 getType' geoids goal = go 10000000000 M.empty geoids $ Seq.fromList [((0,0),1,0)]
    where go best _ _ Seq.Empty = best
          go best seen gids (((x,y),tool,m) Seq.:<| queue)
            | x < 0 || y < 0 =
                go best seen gids' queue
            | ((x,y),tool) `M.member` seen && seen M.! ((x,y),tool) <= m =
                go best seen gids' queue
            | m + (manhattan goal (x,y)) + (if tool == 1 then 0 else 7) >= best =
                go best seen gids' queue
            | tool == typ =
                go best seen gids' queue
            | (x,y) == goal && tool == 1 =
                go (min best m) seen gids' queue
            | otherwise =
                go best (M.insert ((x,y),tool) m seen) gids' queue'
            where (gids',typ) = getType' (x,y) gids
                  ns = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
                  ns' = [(n,tool,m+1) | n <- ns, tool /= typ]
                  ns'' = Seq.fromList $ ns' ++ [((x,y),t,m+7) | t <- [0..2], t /= typ]
                  queue' = queue Seq.>< ns''

manhattan :: Cell -> Cell -> Int
manhattan (a,b) (c,d) = abs (c-a) + abs (d-b)

