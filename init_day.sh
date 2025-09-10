#!/bin/bash

name="day$(printf "%02d\n" $1)"
if test -d "$name"; then
	echo "$name already exists. Exiting.."
	exit 1
fi

# cabal file template
main_template="module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn \$ \"-- Solving $name --\"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn \$ \"Parsed input: \" ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = \"not solved yet\"
    let answer2 = \"not solved yet\"

    -- Print answers
    putStrLn \$ \"Part 1: \" ++ show answer1
    putStrLn \$ \"Part 2: \" ++ show answer2

parse input = input"
cabal_template="
executable $name
    main-is:          Main.hs
    build-depends:    base
                     ,Common
                     ,containers
                     ,split
    hs-source-dirs:   $name
    default-language: Haskell2010
    ghc-options:      -Wall
                      -Wcompat
                      -Widentities
                      -Wincomplete-uni-patterns
                      -Wincomplete-record-updates
                      -Wredundant-constraints
                      -Wpartial-fields
                      -Wunused-packages"
mkdir $name &&
echo "Example input:" &&
example_input=$(</dev/stdin) &&
echo "Using AOC_COOKIE to query for real input..:" &&
aoc_cookie=`cat AOC_COOKIE`
real_input=`curl --cookie "session=$aoc_cookie" https://adventofcode.com/2018/day/$1/input` &&
echo "Successfully curld"
touch "$name/Main.hs" "$name/example_input.txt" "$name/input.txt" &&
echo "$example_input" > "$name/example_input.txt" &&
echo "$real_input" > "$name/input.txt" &&
echo "$main_template" > "$name/Main.hs" &&
echo "
{-
Example input:
$example_input

Real input:
$real_input
-}" >> "$name/Main.hs" &&
echo "$cabal_template" >> advent-of-code.cabal &&
echo "Initialized $name" &&
echo "Started $1 at `date`" >> LOG
cabal run $name &>/dev/null &
gvim -v "$name/Main.hs"
