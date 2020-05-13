#!/usr/bin/sh
## this is used to get the tropical forcing 


count=51

while [ $count -le 51 ]
do


f77 -o 3getTroZDTP.out  3getTroZDTP.f 

ln -s  /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51/FORCE$count.dat_spec  fort.14

3getTroZDTP.out

mv fort.15 /mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/FORCE/TroExtroMF/MF51_Tro/FORCE$count.dat_spec
rm -f *.out
rm -f fort.*

    count=`expr $count + 1`
done

