
ncargf77 -32 plotEOFens.f
#ncargf77 -32  plot-NP-AO.f
#ncargf77 -32  testgloble.f

a.out $mmm
echo plot......
#ctrans -d ps.color gmeta>ps.ps
 ctrans -d ps.mono gmeta>ps.ps
gs   ps.ps
rm -f a.out


