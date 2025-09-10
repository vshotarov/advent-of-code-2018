module Common ( readInput
              , truncateString
              , flattenTuples2
              , everyNth
              , firstWhere
              , firstIdWhere
              , sortBySnd
              , sortByFst
              , unique
              , splitOnceOn
              , mapModify
              , assert
              , doAssert
              , mapTuple
              ) where

import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import qualified Data.Map as M (Map, (!), insert) 
import Data.List (sortBy, isPrefixOf)

readInput :: IO String
readInput = do
    args <- getArgs
    case args of
      [arg]              -> do
          fileExists <- doesFileExist arg
          if fileExists
          then putStrLn "-- Interpreting argument as a file" >> readFile' arg
          else putStrLn "-- Interpreting argument as a string" >> return arg
      ["--fromFile",arg] -> readFile' arg
      ["--raw",arg]      -> return arg
      _                  -> do
          progName <- getProgName
          let inputFilePath = progName ++ "/input.txt"
          putStrLn $ "-- No arg passed. Reading " ++ inputFilePath
          readFile' inputFilePath

readFile' :: String -> IO String
readFile' fp = do
    fileData <- readFile fp
    let asLines = lines fileData
    if length asLines == 1
       then putStrLn "-- Stripping \\n at the end, as the input is one line long"
            >> (return $ head asLines)
       else return fileData

truncateString :: String -> String
truncateString str | length str < 70 = str
                   | otherwise       = take 53 str ++ " ... "
                                    ++ (drop (length str - 13) str)

flattenTuples2 :: [(a,a)] -> [a]
flattenTuples2 []          = []
flattenTuples2 ((x,y):xys) = x:y:flattenTuples2 xys

everyNth :: Int -> [a] -> [a]
everyNth _ []     = []
everyNth n (x:xs) = x:(everyNth n (drop (n-1) xs))

firstWhere :: (a -> Bool) -> [a] -> a
firstWhere _ [] = error "firstWhere: on empty list"
firstWhere predicament (x:_) | predicament x = x
firstWhere predicament (_:xs)                = firstWhere predicament xs

firstIdWhere :: (a -> Bool) -> [a] -> Int
firstIdWhere _ [] = error "firstIdWhere: on empty list"
firstIdWhere predicament xs = go xs 0
    where go [] _ = error "firstIdWhere: no elements satisfy predicament"
          go (x:_) n | predicament x = n
          go (_:xs') n               = go xs' (n+1)

sortByFst :: Ord a => [(a,b)] -> [(a,b)]
sortByFst = sortBy (\(a,_) (b,_) -> compare a b)

sortBySnd :: Ord b => [(a,b)] -> [(a,b)]
sortBySnd = sortBy (\(_,a) (_,b) -> compare a b)

unique :: Eq a => [a] -> [a]
unique = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

splitOnceOn :: Eq a => [a] -> [a] -> ([a],[a])
splitOnceOn [] _ = error "Can't splitOnceOn empty string"
splitOnceOn delimiter xs = case go [] xs of
                             [x] -> (x,[])
                             [x,y] -> (x,y)
                             _ -> error "splitOnceOn split more than once"
        where go buffer [] = [reverse buffer]
              go buffer xs' | isPrefixOf delimiter xs' = (reverse buffer):[xs'']
                  where xs'' = drop (length delimiter) xs'
              go buffer (x:xs') = go (x:buffer) xs'

mapModify :: Ord a => a -> (b -> b) -> M.Map a b -> M.Map a b
mapModify k f m = M.insert k (f $ m M.! k) m

assert :: Bool -> String -> a -> a
assert False msg _ = error msg
assert True _ val  = val

doAssert :: Bool -> String -> IO ()
doAssert False msg = error msg
doAssert True _    = return ()

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)
