module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length . solve $ map S.singleton parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1

type Point = (Int,Int,Int,Int)
type Constellation = S.Set Point

parse :: String -> [Point]
parse input = map (read . (++")") . ("("++)) $ lines input

solve :: [Constellation] -> [Constellation]
solve constellations =
    if length constellations == length state' then state' else solve state'
    where getMembership c cs =
            foldr (\c' (yes,no) -> if areOne c c'
                                   then (c':yes,no)
                                   else (yes,c':no))
                  ([],[]) cs
          foldHelper c cs = (S.union c $ foldr S.union S.empty isIn):isNotIn
              where (isIn,isNotIn) = getMembership c cs
          state' = foldr foldHelper [] constellations

areOne :: Constellation -> Constellation -> Bool
areOne a b = any id [manhattan a' b' <= 3 | a' <- S.toList a, b' <- S.toList b]
    where manhattan (a1,a2,a3,a4) (b1,b2,b3,b4) = sum . map (abs . uncurry (-))
                                                $ zip [a1,a2,a3,a4] [b1,b2,b3,b4]

