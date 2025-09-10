## Advent of Code 2018 solutions in Haskell
To initialize a new day run

```
./init_day.sh {day_num}
```

e.g.

```
./init_day.sh 3
```

which will:

- ask for an example input (after pasting press Ctrl + D)
- ask for the real input (after pasting press Ctrl + D)
- make the directory `day{day_num}`
- write the example input into `day{day_num}/example_input.txt`
- write the real input into `day{day_num}/input.txt`
- write the template into `day{day_num}/Main.hs`
- add the `executable` template into the `advent-of-code.cabal` file

*NOTE: The day_num will be zero padded, so 3 will become 03*

After, that to solve any day, use `cabal run {day} -- {input_file}` like so:

```
cabal run day16 -- day16/example_input.txt
```

Alternatively, you can provide a string directly as the input like so:

```
cabal run day16 -- "this is my actual input"
```

Seems very unlikely, but in the case where a file with the exact same name as
the raw string provided exists, the input will be read from the file, so the
following two arguments have been added in cases like that:

- `--fromFile` - will read the input from the file with the given name
- `--raw` - will read the input directly from the argument
