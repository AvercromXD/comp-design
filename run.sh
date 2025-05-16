#!/bin/bash

# This script is supposed to run your compiler
cabal run l1c-exec $1 output.s
gcc -o $2 output.s
