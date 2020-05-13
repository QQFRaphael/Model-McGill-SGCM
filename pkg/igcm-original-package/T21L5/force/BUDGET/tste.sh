#!/usr/bin/sh
### This is used to change the spectra format to gauss grid
clean

f77 -O3 -static -r8 -o test.out tste.f -L /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77
ln -s /zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/force_clim.dat   fort.13
#ln -s /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE_DJF_ave.dat_crtd   fort.13
test.out

mv fort.9 spec.dat 
#rm -f *.out
#rm -f fort.*
#f77 1.f
#a.out
clean
