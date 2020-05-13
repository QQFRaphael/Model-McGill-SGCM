#!/bin/bash

ifort -convert big_endian -nowarn -r8 flat2jaggedFANOM.f -o flat2jaggedFANOM
ifort -convert big_endian -nowarn -r8 g2s.T31.f -o g2sT31 -L ../lib -l aux
ifort -convert big_endian -nowarn -r8 makeanom.f -o makeanom

./makeanom
./g2sT31 
./flat2jaggedFANOM 

#rm -rf data.ideal
rm -rf gridanom
#rm -rf T31.flatcoeffs
