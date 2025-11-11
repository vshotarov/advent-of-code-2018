module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortOn)
import Data.Tuple (swap)

main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(spaces, units) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let play a = last . Common.takeUntil (isOver . snd) $ iterate (turn a spaces) (0,units)
        outcome (n,state) = n * (sum . map snd $ M.elems state)
    let answer1 = outcome $ play 3
    let answer2 = outcome . snd . last . Common.takeUntil areAllElvesALive
                $ map (\a -> (a,play a)) [4..]
            where numElves = M.size $ M.filter ((=='E') . fst) units
                  areAllElvesALive (_,(_,s)) = (all ((=='E') . fst) $ M.elems s)
                                              && M.size s == numElves

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Unit = (Char, Int)
type State = (M.Map Vec Unit)

parse :: String -> (S.Set Vec, State)
parse input = foldr parseLine (S.empty, M.empty) . zip [0..] $ lines input
    where parseChar y (x,c) acc@(spaces, units)
            | c == '.'      = (S.insert (x,y) spaces, units)
            | c `elem` "GE" = (S.insert (x,y) spaces, M.insert (x,y) (c,200) units)
            | otherwise     = acc
          parseLine (y,line) acc = foldr (parseChar y) acc $ zip [0..] line

turn :: Int -> S.Set Vec -> (Int,State) -> (Int,State)
turn elfAttack spaces (turnI,units) = go units . sort $ M.keys units
    where go s [] = (turnI+1,s)
          go s _ | isOver s = (turnI,s)
          go s (u:nits) = let s' = unitAttack elfAttack $ unitMove spaces s u
                           in go (if u `M.member` s then s' else s) nits

isOver :: State -> Bool
isOver = (==1) . S.size . S.fromList . map fst . M.elems

unitMove :: S.Set Vec -> State -> Vec -> (Vec,State)
unitMove spaces units p
  | p `S.member` adjacentCells = (p,units)
  | otherwise = if null shortestPaths || null firstSteps then (p,units) else (p',units')
  where u@(team, _) = units M.! p
        enemies = M.filter ((/= team) . fst) units
        adjacentCells = S.fromList . concatMap neighbours $ M.keys enemies
        adjacentSpaces = S.toList $ S.intersection adjacentCells spaces
        adjacentFreeSpaces = filter (not . (`M.member` units)) adjacentSpaces
        spaces' = S.insert p . S.difference spaces . S.fromList $ M.keys units
        shortestPaths = filter (not . null ) $ map (getShortestPathToOne spaces' p) adjacentFreeSpaces
        shortestLength = minimum $ map length shortestPaths
        shortestPaths' = filter ((==shortestLength) . length) shortestPaths
        alternatives = concatMap (getAlternatives spaces') shortestPaths'
        firstSteps = sort . S.toList . S.fromList $ map (head . tail) alternatives
        p' = head firstSteps
        units' = M.insert p' u $ M.delete p units

unitAttack :: Int -> (Vec,State) -> State
unitAttack elfAttack (p,units) = if M.null adjacentEnemies then units else units''
    where (team, _) = units M.! p
          enemies = M.filter ((/= team) . fst) units
          adjacentEnemies = M.filterWithKey (\k _ -> (==1) $ manhattan p k) enemies
          (ep,(eteam,hp)) = head . sortOn (\((x,y),(_,h)) -> (h, y, x)) $ M.toList adjacentEnemies
          units' = M.insert ep (eteam,hp-(if team == 'E' then elfAttack else 3)) units
          units'' = M.filter ((>0) . snd) units'

getAlternatives :: S.Set Vec -> [Vec] -> [[Vec]]
getAlternatives _ [] = error "empty path"
getAlternatives spaces path@(p:ath) = path:viable
    where roots = filter (`S.member` spaces) . filter (/= (head ath)) $ neighbours p
          newPaths = map (\r -> p:(getShortestPathToOne spaces r (last path))) roots
          viable = filter ((== length path) . length) newPaths

getShortestPathToOne :: S.Set Vec -> Vec -> Vec -> [Vec]
getShortestPathToOne spaces a b = go S.empty [(a,[])]
    where go _ [] = []
          go seen ((p,path):queue)
            | p == b = reverse (p:path)
            | p `S.member` seen = go seen queue
            | otherwise = go (S.insert p seen) $ queue ++ ns
            where ns = map (\n -> (n,p:path)) . filter (`S.member` spaces) $ neighbours p

neighbours :: Vec -> [Vec]
neighbours (x,y) = [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]

manhattan :: Vec -> Vec -> Int
manhattan (a,b) (c,d) = abs (c-a) + abs (d-b)

sort :: [Vec] -> [Vec]
sort = sortOn swap

asStr :: S.Set Vec -> State -> String
asStr spaces units =
    concatMap (\y -> (map (\x -> fst $ M.findWithDefault (def (x,y)) (x,y) units) [0..40]) ++ "\n")
              [0..40]
    where def (x,y) = (if (x,y) `S.member` spaces then ('.',0) else ('#',0))

draw :: S.Set Vec -> State -> IO ()
draw spaces units = putStrLn $ asStr spaces units

