#!/usr/bin/sh

echo compiling specan.f
f77 -O3 -static -r8 -o specan.out specan.f -L /usr/people/hall/igcm/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77
ln -s /diskc/jiaxj/force/UVTP4849after.dat fort.13


echo running specan.f
specan.out

mv fort.9 UVTP4849after.dat.spec
rm -f fort.*
rm -f specan.out
