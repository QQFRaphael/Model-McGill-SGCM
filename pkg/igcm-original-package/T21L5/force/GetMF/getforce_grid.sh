#!/usr/bin/sh

echo compiling getforce_grid.f
f77 -O3 -static -r8 -o getforce_grid.out getforce_grid.f -L /usr/people/hall/igcm/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77
#ln -s /diska/hlin/igcm/force/ttsp.dat fort.13
ln -s FORCE_51DJF.dat_crtd fort.13
echo running getforce_grid.f
getforce_grid.out

mv fort.9 ttgrid.dat

rm -f *.out
rm -f fort.*


