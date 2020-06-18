#!/bin/csh
#alias ff "f77 -C -e -xl -PIC -lV77 -dalign"
#set wind_lib='-L/home/wind/v1.4a/lib -lWAVES -lV77'
wf77 cdfu.for $wind_lib -o cdfu.exe
#set cdflib='-L/home/wind/cdf/cdf25-dist/lib -lcdf'
#ff cdfu.for $wind_lib $cdflib -o cdfu.exe
