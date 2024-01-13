#!/bin/sh

if [ -d "build" ]; then
   make build-project
else
   make generate
fi

make run
echo

z3 -smt2 solver.smt2