#!/bin/bash

# This script is supposed to run your compiler
cabal run l1c-exec $1 output.s

haskell_exit_code=$?

# If the Haskell program failed, then exit with its error code
if [ $haskell_exit_code -ne 0 ]; then
    echo "Haskell program failed with exit code $haskell_exit_code. Aborting gcc compilation."
    exit $haskell_exit_code
fi

gcc -o $2 output.s

gcc_exit_code=$?

if [ $gcc_exit_code -ne 0 ]; then
    echo "GCC failed with exit code $gcc_exit_code."
    exit $gcc_exit_code
fi

echo "Compilation succeeded."
exit 0
