#!/bin/sh

set -aex

rm -rf ./toGaussian ./specan
ifort -convert big_endian -r8 -nowarn -o specan specan.f -L /home/qqf/igcm/lib -lblas -lfft -lutil
ifort -convert big_endian -r8 -nowarn -o toGaussian toGaussian.f -L /home/qqf/igcm/lib -lblas -lfft -lutil


[ ! -d data ]   &&   mkdir -p data

mv specan toGaussian data/

count=40
while [ $count -le 40 ]
do
  year1=`expr 1978 + $count`

  echo $year1

  export YEAR1=$year1

cd data
rm -rf testt.dat testu.dat testv.dat testz.dat out.dat fort.13
cd ..

  ncl getUVTP.ncl

cd data
./toGaussian $year1

mv out.dat fort.13
./specan $year1

mv ZDTP.dat.spec ZDTP-${year1}SON.spec

cd ..
 count=`expr $count + 1`
done

rm -rf data
