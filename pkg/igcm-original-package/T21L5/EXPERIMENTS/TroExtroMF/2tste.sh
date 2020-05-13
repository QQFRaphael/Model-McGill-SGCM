#!/usr/bin/sh
## this is used to change gauss grid to spectra format


clean

count=51

while [ $count -le 51 ]
do


f77 -O3 -static -r8 -o tste.out tste.f -L /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/lib \
   -l sgifft1 -l sgiutil1 || ABORT f77

echo /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51/FORCE$count.dat
ln -s  /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51/FORCE$count.dat  fort.13

tste.out

mv fort.9 /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51/FORCE$count.dat_spec
rm -f *.out
rm -f fort.*

    count=`expr $count + 1`
done

