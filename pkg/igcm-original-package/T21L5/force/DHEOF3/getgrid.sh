#!/usr/bin/sh

echo compiling getgrid.f
f77 -O3 -static -r8 -o getgrid.out getgrid.f -L /usr/people/hall/igcm/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77
#ln -s /diskc/jiaxj/force/HaiForce/TroTrend.dat fort.13
#ln -s /diskc/jiaxj/force/HaiForce/haiclimate fort.13
#ln -s  /zemo2/jiaxj/data/FORCE/FORCE_DJF15.dat fort.13
ln -s test.dat fort.13
echo running getforce_grid.f
getgrid.out

mv fort.9 grid.dat

rm -f *.out
rm -f fort.*
f77 getVerAve.f
a.out

#vi fort.33
