# Setup

Build the program with `make`, from the project directory.

# Usage

Various example programs (including the initial 5) are available in `src/Tests.hs`. You may select example programs to compile by passing their ID to the program:

    src/Main 1

A script is provided to automate compiling and executing a given example program, and print the output to the terminal:

    ./run.sh 1
    # output: 6

The `runall.sh` script compiles and runs all TODO test programs, comparing their outputs to the expected values in `expected.txt`:

    ./runall.sh TODO
