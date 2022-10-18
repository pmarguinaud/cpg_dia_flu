#!/bin/bash

set -x

for f in compute/*.F90
do 

  b=$(basename $f)
  rm compile.cpu_intel/$b

done
