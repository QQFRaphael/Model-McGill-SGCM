#!/usr/bin/sh
## this is used to change gauss grid to spectra format


clean

count=51

while [ $count -le 51 ]
do


f77 -O3 -static -r8 -o test.out test.f -L /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/lib \
   -l sgifft1 -l sgiutil1 || ABORT f77

echo /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51_Tro/FORCE$count.dat
ln -s  /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51_Tro/FORCE$count.dat_spec  fort.13

test.out

mv fort.9 /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51_Tro/FORCE$count.dat
rm -f *.out
rm -f fort.*

    count=`expr $count + 1`
done

