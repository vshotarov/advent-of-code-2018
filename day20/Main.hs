module Main where

import qualified Common
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let allPaths = map toList $ paths (0,0) parsedInput
        gridMap = pathsToMap $ map toList allPaths
        pathLengths = M.elems $ fill gridMap
    let answer1 = maximum pathLengths
    let answer2 = length $ filter (>=1000) pathLengths

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Map = M.Map Vec [Vec]

parse :: String -> String
parse = init . tail

fill :: Map -> M.Map Vec Int
fill mmap = go M.empty S.empty [((0,0),0)]
    where go out _ [] = out
          go out seen ((c,d):queue)
            | c `S.member` seen = go out seen queue
            | otherwise = let ns = mmap M.! c
                           in go (M.insert c d out) (S.insert c seen)
                            $ queue ++ map (\n -> (n,d+1)) ns

pathsToMap :: [[Vec]] -> Map
pathsToMap = M.map nub . foldr (\x acc -> foldr insertConnection acc $ toSteps x) M.empty
    where insertConnection (a,b) = M.insertWith (++) b [a] . M.insertWith (++) a [b]
          toSteps x = zip x $ tail x

paths :: Vec -> String -> [Seq.Seq Vec]
paths _ [] = []
paths sp dirs = go [Seq.fromList [sp]] dirs
    where getLast s = case Seq.viewr s of
                          Seq.EmptyR -> error "Accessing end of empty sequence"
                          a Seq.:> x -> (a,x)
          go buffers [] = buffers
          go buffers ('(':irs) = go buffers' irs'
              where inside = getBracketsContents ('(':irs)
                    irs' = drop (length inside + 1) irs
                    routes = getOptions inside
                    concatChildPaths b = map (buffe Seq.><)
                                       $ concatMap (paths r) routes
                        where (buffe,r) = getLast b
                    buffers' = concatMap concatChildPaths buffers
          go buffers (d:irs) = go (map step buffers) irs
                where step b = b Seq.|> (move d . snd $ getLast b)

getBracketsContents :: String -> String
getBracketsContents [] = []
getBracketsContents ('(':remainder) = go (0::Int) remainder
    where go _ [] = error "reached end in looking for bracket contents"
          go b ('(':xs) = '(':(go (b+1) xs)
          go b (')':xs) = if b == 0 then [] else (')':(go (b-1) xs))
          go b (x:xs) = x:(go b xs)
getBracketsContents (x:_) = error $ "calls to getBracketsContents should start with (, not " ++ [x]

getOptions :: String -> [String]
getOptions [] = []
getOptions s = go (0::Int) Seq.empty s
    where go 0 b [] = [toList b]
          go _ _ [] = error "bad input"
          go 0 b ('|':xs) = (toList b):(go 0 Seq.empty xs)
          go l b ('(':xs) = go (l+1) (b Seq.|> '(') xs
          go l b (')':xs) = go (l-1) (b Seq.|> ')') xs
          go l b (x:xs) = go l (b Seq.|> x) xs

move :: Char -> Vec -> Vec
move 'N' (x,y) = (x,y-1)
move 'E' (x,y) = (x+1,y)
move 'S' (x,y) = (x,y+1)
move 'W' (x,y) = (x-1,y)
move c _ = error $ "Unrecognised dir: " ++ [c]


asStr :: Map -> String
asStr mmap = concat [[toChar (x,y) | x <- [minX..maxX]] ++ "\n" | y <- [minY..maxY]]
    where mmap' = M.fromList $ map (\((x,y),cs) -> ((x*2,y*2),map (\(x',y') -> (x'*2,y'*2)) cs)) $ M.toList mmap
          ps = M.keys mmap'
          (xs,ys) = unzip ps
          (minX,maxX,minY,maxY) = (minimum xs, maximum xs, minimum ys, maximum ys)
          toChar p@(x,y)
            | (x,y) == (0,0) = 'X'
            | M.member p mmap' = '.'
            | M.member (x-1,y) mmap' && (x+1,y) `elem` (mmap' M.! (x-1,y)) = '|'
            | M.member (x,y+1) mmap' && (x,y-1) `elem` (mmap' M.! (x,y+1)) = '-'
            | otherwise = '#'

