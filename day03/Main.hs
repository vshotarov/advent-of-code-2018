module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let intersections = map (uncurry intersect) $ Common.pairs parsedInput
    let answer1 = S.size $ foldMap rasterise intersections
    let answer2 = filter (\(Rect i _ _) -> not $ S.member i intersectionIds) parsedInput
            where intersectionIds = S.fromList
                                  $ concatMap (decompId . fmap claimId) intersections

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int, Int)
data Rect = Rect {claimId :: Int, position :: Vec, size :: Vec}
            deriving (Show, Eq)

readVec :: String -> String -> Vec
readVec sep = Common.mapTuple read . Common.splitOnceOn sep

parse :: String -> [Rect]
parse = map parseOne . lines
    where parseOne :: String -> Rect
          parseOne x = case words x of
                         [i,_,p,s] -> Rect (read $ tail i)
                                           (readVec "," $ init p)
                                           (readVec "x" s)
                         _ -> error ("unercognised format " ++ x)

compId :: Int -> Int -> Int
compId a b = 10000*a + b

decompId :: Maybe Int -> [Int]
decompId Nothing = []
decompId (Just i) = [i `div` 10000, i `mod` 10000]

intersect :: Rect -> Rect -> Maybe Rect
intersect (Rect aid (ax,ay) (aw,ah)) (Rect bid (bx,by) (bw,bh))
  | ax >= bx + bw || bx >= ax + aw || ay >= by + bh || by >= ay + ah = Nothing
  | otherwise = Just $ Rect (compId aid bid) (gx,gy) (w,h)
  where (gx,w) = ((max ax bx),(min (ax+aw) (bx+bw)) - (max ax bx))
        (gy,h) = ((max ay by),(min (ay+ah) (by+bh)) - (max ay by))

rasterise :: Maybe Rect -> S.Set Vec
rasterise Nothing = S.empty
rasterise (Just (Rect _ (x,y) (w,h))) = S.fromList
    [(x',y') | x' <- [x..x+w-1], y' <- [y..y+h-1]]

