#!/bin/bash

BENCHMARK="Mandelbrot"

while true
do
#    RUST_LOG=som_interpreter_bc::gc=debug cargo run --bin ${EXE:=som-interpreter-bc} --features=$COMPILATION_FLAGS -- -c core-lib/Smalltalk core-lib/Examples/Benchmarks core-lib/Examples/Benchmarks/LanguageFeatures core-lib/Examples/Benchmarks/Json core-lib/Examples/Benchmarks/Richards core-lib/Examples/Benchmarks/DeltaBlue -- core-lib/Examples/Benchmarks/BenchmarkHarness.som $BENCHMARK 1 7
    RUST_LOG=som_interpreter_bc::gc=debug cargo run --bin ${EXE:=som-interpreter-bc} --features=$COMPILATION_FLAGS -- -c core-lib/Smalltalk core-lib/Examples/Benchmarks  -- Foo
    echo -ne "\n"
done
