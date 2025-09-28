module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let bbox = toBBox parsedInput
        internal = filter (isInsideBBox bbox) parsedInput
    let answer1 = maximum $ map (\c -> toArea bbox $ filterF c) internal
            where filterF c c' = all (>d) distances
                      where allExceptC = filter (/= c) parsedInput
                            distances = map (manhattan c') allExceptC
                            d = manhattan c c'
    let answer2 = toArea bbox (\c -> (sum $ map (manhattan c) parsedInput) < 10000)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Coords = (Int,Int)
type BBox = ((Int,Int),(Int,Int))

parse :: String -> [Coords]
parse input = map (Common.mapTuple read . Common.splitOnceOn ",") $ lines input

toBBox :: [Coords] -> BBox
toBBox coords = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    where (xs,ys) = unzip coords

isInsideBBox :: BBox -> Coords -> Bool
isInsideBBox ((minX,minY),(maxX,maxY)) (x,y) =
    x > minX && x < maxX && y > minY && y < maxY

toArea :: BBox -> (Coords -> Bool) -> Int
toArea ((minX,minY),(maxX,maxY)) filterF =
    length $ [(x',y') | x' <- [minX..maxX], y' <- [minY..maxY], filterF (x',y')]

manhattan :: Coords -> Coords -> Int
manhattan (ax,ay) (bx,by) = (abs (ax-bx)) + (abs (ay-by))

