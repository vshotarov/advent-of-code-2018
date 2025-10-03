module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (tree,_) = toTree parsedInput
    let answer1 = foldTree (\n acc -> acc + sum (metadata n)) 0 tree
    let answer2 = treeValue tree

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Tree = Tree { children :: [Tree], metadata :: [Int] }
          deriving Show

parse :: String -> [Int]
parse = map read . words

toTree :: [Int] -> (Tree, [Int])
toTree (c:m:queue) = (Tree (reverse children') meta, remainder')
    where go 0 nodes numbers = (nodes, numbers)
          go i nodes numbers = let (node, r) = toTree numbers
                                in go (i-1) (node:nodes) r
          (children', remainder) = go c [] queue
          (meta, remainder') = splitAt m remainder
toTree _ = error "bad input"

foldTree :: (Tree -> a -> a) -> a -> Tree -> a
foldTree f acc tree@(Tree c _) = f tree (foldr (flip $ foldTree f) acc c)

treeValue :: Tree -> Int
treeValue (Tree [] m) = sum m
treeValue (Tree c m ) = sum [v !! (i-1) | i <- m, i /= 0, i <= length c]
    where v = map treeValue c

