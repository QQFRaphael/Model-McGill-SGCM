#!/usr/bin/sh

echo compiling getforce_UV_grid.f


f77 -O3 -static -r8 -o getforce_UV_grid.out getforce_UV_grid.f -L /usr/people/hall/igcm/lib  \
   -l sgifft1 -l sgiutil1 || ABORT f77
ln -s /zemo2/jiaxj/force/BUDGET/DH.dat_crtd   fort.13


echo running getforce_UV_grid.f
getforce_UV_grid.out

mv fort.9 UV_grid.dat

rm -f *.out
rm -f fort.*


