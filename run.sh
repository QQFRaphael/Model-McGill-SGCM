#!/bin/sh
set -xve
#-----------------------------------------#
# Run Reading Spectral Model.             #
# 100. climate mode.                      #
# T21L5   ***GLOBAL***                    #
# Read full history initial data.         #
# If training run set LTRAIN=.T. in       #
# namelist and read data.seq into chan 10 #
#-----------------------------------------#
set +v
#
############################################# USER SWITCHES.
#
KD=/home/qqf/igcm        # Root path for model files
KD2=$KD/data
EXPID=control
EXPDIR=$KD/$EXPID  # Experiment directory.
COMPILE=yes                     # Nupdate and compile (yes/no)
RUN=yes                         # run an executable already created
                                # (yes/no)
EXEC=$KD/$EXPID/prog  # Full path of the executable 
#                               # either to be created if COMPILE=yes
#                               # or to be run if COMPILE=no
#
############################################# UPDATE DIRECTIVES.
#
[ ! -d $EXPDIR ]   &&   mkdir -p $EXPDIR
#
############################################# COMPILE AND RUN PROGRAM.
#
if [ $COMPILE = yes ]
then
$KD/lib/nupdate -p $KD/igcm1.npl  -c igcm1 -i $KD/updates -f  -w 72  -o sq 
ifort  -convert big_endian -nowarn -O3 -static -r8 -o $EXEC igcm1.f -L $KD/lib -l fft -l blas -l util
rm igcm1.f
fi

#
if [ $RUN = yes ]
then
    ln  -s $KD/namelist  fort.7
#   ln  -s $EXPDIR/restart.11.9000d.11-18.R0.5  fort.10
    ln  -s $KD2/ZDTPdjf.dat.spec  fort.10
    ln  -s $KD2/FORCE_DJFmean54yr.dat  fort.13
    ln  -s $KD2/data.ideal  fort.15
    ln  -s $KD/geog/MG96JG24.orog.b  fort.18
    ln  -s $KD/geog/MG96JG24.mask.b  fort.19
#
    ln  -s $EXPDIR/history  fort.9
    ln  -s $EXPDIR/restart.11  fort.11
    ln  -s $EXPDIR/restart.12  fort.12
    ln  -s $EXPDIR/results  fort.2
#
    $EXEC 
    cat fort.2
    rm fort.* #updates namelist
fi
