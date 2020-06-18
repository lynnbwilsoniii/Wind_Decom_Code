#!/bin/csh
alias ff "f77 -C -e -xl -PIC -lV77 -dalign"
set wind_lib='-L/home/wind/v1.4a/lib2 -lWAVES -lV77'
set cdflib='-L/home/wind/cdf/cdf25-dist/lib -lcdf'
ff cdf.for $wind_lib -o cdf.exe
#ff cdf.for $wind_lib $cdflib -o cdf.exe
