#!/bin/bash

./scripts/compile.pl --arch cpu_intel --update --compile  --inline 
./scripts/compile.pl --arch cpu_intel --update --compile  

for i in 0 1
do
./compile.cpu_intel.$i/wrap_cpg_dia_flux.x --case ./t0031 > diff.$i.txt 2>&1
done

