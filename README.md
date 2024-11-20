# Setup

Build the program with `make`, from the project directory.

# Usage

Various example programs (including the initial 5) are available in `src/Tests.hs`. You may select example programs to compile by passing their ID to the program:

    src/Main 1

A script is provided to automate compiling and executing a given example program, and print the output to the terminal:

    ./run.sh 1
    # output: 6

The `tests.sh` script compiles and runs all test programs up to and including the given ID, comparing their outputs to the expected values in `expected.txt`. There are 29 total test cases:

    ./tests.sh 29

To interact directly with the compiler (e.g. to compile self-defined programs), use `ghci`:

    cd src && ghci Main
    > template <- readFile "template.java"
    > toJava template $ testProg 1 -- or your own
