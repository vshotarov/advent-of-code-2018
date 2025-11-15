module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Bits

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(ipRegister,program) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let initState = (M.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)])
    let answer1 = (M.! 0) . snd . last $ takeWhile (not . shouldHalt program) states
            where states = iterate (step ipRegister program) (0, initState)
    -- running through the instructions by hand, i see that the program finds
    -- the sum of all divisors of the number in regster 3 (D)
    let answer2 = foldr (\x -> (x+(regDAfter100 `div` x)+)) 0 smallDivisors
            where states = iterate (step ipRegister program) (0, M.insert 0 1 initState)
                  -- run for 100 steps to perform the initialisation steps which
                  -- populate register 3 (D) with a large number
                  regDAfter100 = (snd $ states !! 100) M.! 3
                  bound = round . sqrt $ fromIntegral regDAfter100
                  smallDivisors = filter ((==0) . (regDAfter100 `mod`)) [1..bound]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = M.Map Int Int
type Instruction = (String,Int,Int,Int)
type Program = [Instruction]

parse :: String -> (Int, Program)
parse input = (read $ drop 4 ip, map parseInstruction program)
    where (ip:program) = lines input
          parseInstruction line = (op,read a,read b,read c)
              where [op,a,b,c] = splitOn " " line

solve :: Int -> Program -> Int -> State -> State
solve ipr program ip state
  | ip < 0 || ip >= length program = state
  | otherwise = solve ipr program ip' state''
  where i = program !! ip
        state' = M.insert ipr ip state
        state'' = perform i state'
        ip' = 1 + state'' M.! ipr

shouldHalt :: Program -> (Int,State) -> Bool
shouldHalt p (i,_) = i < 0 || i >= length p

step :: Int -> Program -> (Int,State) -> (Int,State)
step ipr program (ip,state) = (ip', state'')
  where i = program !! ip
        state' = M.insert ipr ip state
        state'' = perform i state'
        ip' = 1 + state'' M.! ipr

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

