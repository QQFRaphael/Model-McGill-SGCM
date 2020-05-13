#!/usr/bin/sh
### this is used to change the spectra formate to gaussgrid

rm fort.*
f77 -O3 -static -r8 -o test.out test.f -L /usr/people/hall/igcm/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77

#ln -s /zemo2/jiaxj/igcm/HaiforceSVD2/GetZDTP/ZDTP_Ens.dat    fort.13
#ln -s /zemo2/jiaxj/force/BUDGET/DH.dat_crtd	fort.13
#ln -s spec.dat fort.13
#ln -s /zemo2/jiaxj/data/HaiForce/FORCE_DJF15.dat fort.13

test.out

mv fort.9 gauss.dat 
rm -f *.out
rm -f fort.*
#f77 getVerAve.f
#a.out


