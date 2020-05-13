#!/bin/sh

#script to get Z500
#convert history file to single precision
#
BEGDAY=1.
ENDDAY=90.
ISTEP=1
ITSTEPS=64

echo 'compiling getZ500pre.f'
ifort -o getZ500pre getZ500pre.f -convert big_endian

INFILE=/home/qqf/igcm/control/history

echo 'running getZ500pre'

cat << /EOF | ./getZ500pre $INFILE
$BEGDAY
$ENDDAY
$ISTEP
/EOF


#prepare input to presmap
#
cat << /EOF > temphead
COMMON
  TEMPDIR=/home/qqf/igcm/smap
  HOMEDIR=/home/qqf/igcm/smap
  CURRENTDIR=/home/qqf/igcm/smap
  BEGDAY=$BEGDAY
  ENDDAY=$ENDDAY
  DIAGNOSTICS=SG
  UTF=1.4
  LEVELTYPE=IS
  PRINT=T
  PLOT=F
  PRINTSIGFIG=3
  ASCIICHARS=2
  SCRATCH=F
  OROGRAPHY=F
  FREQSAMPLING=$ITSTEPS
  FREQPRINT=$ITSTEPS
/EOF
cat temphead presmap11.Z500SG > fort.10
#
#run presmap and smap
#
echo running presmap
./presmap11.out
ln -s data.res.sp fort.9

echo running smap
./smap11.out
mv gridout Z500_monthly.dat
mv fort.8 titles_Z500_monthly
mv nrecsout nrecs_Z500_monthly
rm fort.*
#
