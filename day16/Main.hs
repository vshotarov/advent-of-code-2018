module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.Tuple (swap)

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(samples,testProgram) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ filter ((>=3) . S.size . findMatchingOpcodes) samples
    let answer2 = foldl (\acc x -> perform x acc) (M.fromList [(0,0),(1,0),(2,0),(3,0)])
                $ map (\(op,a,b,c) -> (mappings M.! op,a,b,c)) testProgram
            where mappings = unobscureInstructions samples

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = M.Map Int Int
type ObscuredInstruction = (Int,Int,Int,Int)
type Instruction = (String,Int,Int,Int)
type Sample = (State, ObscuredInstruction, State)

parse :: String -> ([Sample],[ObscuredInstruction])
parse input = (map parseOne $ splitOn [""] samples, map readIns testProgram)
    where (samples,testProgram) = Common.splitOnceOn ["","",""] $ lines input
          readIns l = case (map read $ splitOn " " l) of
                        [op,a,b,c] -> (op,a,b,c)
                        _ -> error "invalid input"
          parseOne ls = let registersPre = read . drop 8 $ ls !! 0
                            ins = readIns $ ls !! 1
                            registersPost = read . drop 8 $ ls !! 2
                            toState = M.fromList . zip [0..]
                         in (toState registersPre, ins, toState registersPost)

unobscureInstructions :: [Sample] -> M.Map Int String
unobscureInstructions samples = go M.empty M.empty samples
    where go solved mappings []
            | M.size solved == 16 = M.fromList . map swap $ M.toList solved
            | otherwise = go solved mappings samples
          go solved mappings (s@(_,(obscOp,_,_,_),_):amples)
            | otherwise = go solved' mappings'' amples
              where matching = S.filter (not . (`M.member` solved)) $ findMatchingOpcodes s
                    matched = head $ S.toList matching
                    mappings' = M.insertWith S.intersection obscOp matching mappings
                    (solved',mappings'') = if S.size matching == 1
                                              then (M.insert matched obscOp solved,
                                                    M.map (S.delete matched) mappings')
                                              else (solved,mappings')

findMatchingOpcodes :: Sample -> S.Set String
findMatchingOpcodes (pre,(_,a,b,c),post) = S.fromList $ filter doesMatch allOps
    where doesMatch op = post == perform (op,a,b,c) pre

allOps :: [String]
allOps = ["addr","addi","mulr","muli","banr","bani","borr","bori"
         ,"setr","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"]

perform :: Instruction -> State -> State
perform ("addr",a,b,c) s = M.insert c (s M.! a + s M.! b) s
perform ("addi",a,b,c) s = M.insert c (s M.! a + b) s
perform ("mulr",a,b,c) s = M.insert c (s M.! a * s M.! b) s
perform ("muli",a,b,c) s = M.insert c (s M.! a * b) s
perform ("banr",a,b,c) s = M.insert c ((s M.! a) .&. (s M.! b)) s
perform ("bani",a,b,c) s = M.insert c ((s M.! a) .&. b) s
perform ("borr",a,b,c) s = M.insert c ((s M.! a) .|. (s M.! b)) s
perform ("bori",a,b,c) s = M.insert c ((s M.! a) .|. b) s
perform ("setr",a,_,c) s = M.insert c (s M.! a) s
perform ("seti",a,_,c) s = M.insert c a s
perform ("gtir",a,b,c) s = M.insert c (if a > (s M.! b) then 1 else 0) s
perform ("gtri",a,b,c) s = M.insert c (if (s M.! a) > b then 1 else 0) s
perform ("gtrr",a,b,c) s = M.insert c (if (s M.! a) > (s M.! b) then 1 else 0) s
perform ("eqir",a,b,c) s = M.insert c (if a == (s M.! b) then 1 else 0) s
perform ("eqri",a,b,c) s = M.insert c (if (s M.! a) == b then 1 else 0) s
perform ("eqrr",a,b,c) s = M.insert c (if (s M.! a) == (s M.! b) then 1 else 0) s
perform ins _ = error $ "Unrecognised instruction " ++ show ins

