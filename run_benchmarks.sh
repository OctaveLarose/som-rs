#!/bin/bash

BENCHMARKS=("Bounce" "Mandelbrot" "List" "Permute" "Queens" "QuickSort" "Sieve" "Fannkuch" "JsonSmall" "DeltaBlue" "Richards")
EXE=som-interpreter-ast

for bench in "${BENCHMARKS[@]}"
do
    cargo run --bin $EXE -- -c core-lib/Smalltalk core-lib/Examples/Benchmarks core-lib/Examples/Benchmarks/Json core-lib/Examples/Benchmarks/Richards core-lib/Examples/Benchmarks/DeltaBlue -- core-lib/Examples/Benchmarks/BenchmarkHarness.som $bench 1 7
    echo -ne "\n"
done
