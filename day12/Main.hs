module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(state,rules) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states = iterate (step rules) state
        calcTotal ofs pots = sum . map fst . filter ((=='#') . snd) $ zip [ofs,ofs+1..] pots
    let answer1 = calcTotal ofs pots
            where (ofs,pots) = states !! 20
    let answer2 = calcTotal (50000000000 - i' + ofs) pots
            where getFirstRepeating _ [] = error "no states"
                  getFirstRepeating _ [_] = error "no states"
                  getFirstRepeating i ((_,p1):t@(_,p2):ates)
                    | p1 == p2 = (i,t)
                    | otherwise = getFirstRepeating (i+1) (t:ates)
                  (i',(ofs,pots)) = getFirstRepeating 1 states

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = (Int,String)
type Ruleset = S.Set String

parse :: String -> (State, Ruleset)
parse input = case lines input of
                (firstLine:_:lines') -> (toState firstLine, toRuleset lines')
                _ -> error "bad input"
    where toState line = (0, snd $ Common.splitOnceOn ": " line)
          toRuleset = S.fromList . map (take 5) . filter (\l -> (last l) == '#')

step :: Ruleset -> State -> State
step rules (startOfs,startPots) = stripEmpty $ go ("...." ++ startPots ++ "....")
    where stripEmpty pots = (startOfs-2+(length pots)-(length pots'),
                             reverse . dropWhile (=='.') $ reverse pots')
                where pots' = dropWhile (=='.') pots
          go pots = plant:next
                where plant = if S.member (take 5 pots) rules then '#' else '.'
                      next = if length pots > 5 then (go $ tail pots) else []

