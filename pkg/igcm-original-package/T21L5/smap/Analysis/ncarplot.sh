#/usr/bin/sh

ncargf77 -32  plotEOFens.f


a.out
echo plot......
#ctrans gmeta

ctrans -d ps.color gmeta>ps.ps
#ctrans -d ps.mono gmeta>ps.ps
#lp -d jet ps
gs	ps.ps
#ctrans gmeta
#rm -f a.out
#rm -f gmeta



