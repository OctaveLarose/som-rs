#!/bin/bash

BENCHMARKS=("Bounce" "Mandelbrot" "TreeSort" "List" "Permute" "Queens" "IntegerLoop" "QuickSort" "Sieve" "Fannkuch" "JsonSmall" "DeltaBlue" "Richards" "Towers")
#COMPILATION_FLAGS="inlining-disabled"

# max log level to not print anything
export RUST_LOG=error

for bench in "${BENCHMARKS[@]}"
do
    cargo run --bin ${EXE:=som-interpreter-ast} --features=$COMPILATION_FLAGS -- -c core-lib/Smalltalk core-lib/Examples/Benchmarks core-lib/Examples/Benchmarks/LanguageFeatures core-lib/Examples/Benchmarks/Json core-lib/Examples/Benchmarks/Richards core-lib/Examples/Benchmarks/DeltaBlue -- core-lib/Examples/Benchmarks/BenchmarkHarness.som $bench 1 7
    echo -ne "\n"
done
