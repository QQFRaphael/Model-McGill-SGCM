#!/usr/bin/sh

rm fort.*
echo compiling getforce_spZD
f77 -O3 -static -r8 -o getforce_spZD.out getforce_spZD.f -L /usr/people/hall/igcm/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77

 ln -s /diskc/jiaxj/force/HaiForce/TroTrend.dat_gauss  fort.13
#ln -s gauss.dat fort.13

echo running getforce_spZD
getforce_spZD.out

mv fort.9 spec.dat 
