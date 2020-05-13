#!/usr/bin/sh
## this is used to change gauss grid to spectra format
clean

f77 -O3 -static -r8 -o test.out test.f -L /mnt/climate/data/loach/jiaxj/Data/SGCM/lib \
   -l sgifft1 -l sgiutil1 || ABORT f77

#ln -s  /zemo2/jiaxj/igcm/EXPERIMENTS/SVD2_Detrend/tro.dat	  fort.13
ln -s  /zemo2/jiaxj/igcm/EXPERIMENTS/IDEAL/pert.dat	  fort.13


test.out

#mv fort.9 /zemo2/jiaxj/igcm/EXPERIMENTS/SVD2_Detrend/spec.dat 
mv fort.9 /zemo2/jiaxj/igcm/EXPERIMENTS/IDEAL/spec.dat 
rm -f *.out
clean

