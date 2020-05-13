ifort -convert big_endian -g -static -nowarn -o smap11.out smap11T31.f ismax.f ismin.f sdot.f ssum.f sublib5.ecmwf.f 
ifort -convert big_endian -g -static -nowarn -o presmap11.out presmap11.f
rm -rf *.o
mv *.out ../
