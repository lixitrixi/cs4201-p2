#!/bin/bash

src/Main $1 > Prog.java && javac Prog.java && java Prog
