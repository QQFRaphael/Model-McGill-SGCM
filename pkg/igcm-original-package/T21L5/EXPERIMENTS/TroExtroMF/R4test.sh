#!/usr/bin/sh
## this is used to change gauss grid to spectra format


clean


f77 -O3 -static -r8 -o test.out test.f -L /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/lib \
   -l sgifft1 -l sgiutil1 || ABORT f77

ln -s  tro.dat  fort.13

test.out

mv fort.9 spec.dat
rm -f *.out
rm -f fort.*


