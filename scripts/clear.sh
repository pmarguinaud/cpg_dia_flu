#!/bin/bash

set -x

for f in compute/*.F90
do 

  b=$(basename $f .F90)
  rm compile.cpu_intel/$b*

done
