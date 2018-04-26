#!/bin/sh
gfortran -o  fin_gen fin.f90
gfortran -o  fin_gen_nointsct_trim fin_nointsct_trim.f90
gfortran -o  hull_gen hull.f90
gfortran -o  sail_gen sail.f90

./fin_gen
./fin_gen_nointsct_trim
./hull_gen
./sail_gen
