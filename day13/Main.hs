module Main where

import qualified Common
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Tuple (swap)

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(grid,carts) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states = iterate (step grid . snd) ([],carts)
    let answer1 = head . fst . last $ Common.takeUntil (not . null . fst) states
    let answer2 = head . snd . last $ Common.takeUntil ((==1) . length . snd) states

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

    --mapM_ (draw grid . snd) states

type Point = (Int,Int)
type Grid = M.Map Point Char
type CartState = (Point, (Char, Int))
type State = [CartState]

parse :: String -> (Grid, State)
parse input = (M.fromList grid', map (\(p,c) -> (p,(c,0))) carts)
    where grid = lines input
          (maxX,maxY) = ((length $ head grid) - 1, length grid - 1)
          grid' = [((x,y), grid !! y !! x) | y <- [0..maxY], x <- [0..maxX]]
          carts = [((x,y),c) | y <- [0..maxY], x <- [0..maxX],
                               let c = grid !! y !! x,
                               c `elem` "<>v^"]

collisions :: State -> [Point]
collisions carts = go [] [] carts
    where go _ cs [] = cs
          go seen cs ((p,_):carts')
            | p `elem` seen = go seen (p:cs) carts'
            | otherwise = go (p:seen) cs carts'
removeCollisions :: State -> ([Point], State)
removeCollisions state = (cs, filter (not . (`elem` cs) . fst) state)
    where cs = collisions state

step :: Grid -> State -> ([Point], State)
step grid state = go [] [] $ sortOn (swap . fst) state
    where go collided out [] = (collided,out) 
          go collided out (c:cs) =
              case (collisions (c':out), collisions (c':cs)) of
                ([], []) -> go collided (c':out) cs
                (x, []) -> go (collided ++ x) (clean x out) cs
                ([], x) -> go (collided ++ x) out (clean x cs)
                _ -> error "collision with both processed and unprocessed (should be impossible)"
              where c' = stepOne grid c
                    clean a b = filter (not . (`elem` a) . fst) b

move :: Point -> Char -> Point
move (x,y) '^' = (x,y-1)
move (x,y) '>' = (x+1,y)
move (x,y) 'v' = (x,y+1)
move (x,y) '<' = (x-1,y)
move _ d = error $ "bad direction in move " ++ [d]

stepOne :: Grid -> CartState -> CartState
stepOne grid (p, (dir, counter))
    | c == '+'  = (move p crossingDir, (crossingDir, counter+1))
    | c == '/'  = (move p cornerADir, (cornerADir, counter))
    | c == '\\' = (move p cornerBDir, (cornerBDir, counter))
    | otherwise = (move p dir, (dir, counter))
    where c = grid M.! p
          turn = ([turnLeft, id, turnRight] !! (counter `mod` 3))
          crossingDir = turn dir
          cornerADir = case dir of
                         '^' -> '>'
                         '>' -> '^'
                         'v' -> '<'
                         '<' -> 'v'
                         d -> error $ "bad direction in stepOn " ++ [d]
          cornerBDir = case dir of
                         '^' -> '<'
                         '>' -> 'v'
                         'v' -> '>'
                         '<' -> '^'
                         d -> error $ "bad direction in stepOn " ++ [d]

turnLeft :: Char -> Char
turnLeft '^' = '<'
turnLeft '>' = '^'
turnLeft 'v' = '>'
turnLeft '<' = 'v'
turnLeft d = error $ "bad input direction to turnLeft " ++ [d]

turnRight :: Char -> Char
turnRight '^' = '>'
turnRight '>' = 'v'
turnRight 'v' = '<'
turnRight '<' = '^'
turnRight d = error $ "bad input direction to turnRight " ++ [d]

draw :: Grid -> State -> IO ()
draw grid state = putStrLn . concat
                $ [([M.findWithDefault ' ' (x,y) grid' | x <- [0..maxX]] ++ "\n")
                  | y <- [0..maxY]]
    where maxX = maximum . map (fst . fst) $ M.toList grid
          maxY = maximum . map (snd . fst) $ M.toList grid
          grid' = foldr (\(p,(d,_)) -> M.insert p d) grid state

