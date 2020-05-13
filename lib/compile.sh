for dir in 'abort' 'au_' 'blas' 'fft' 'util'
do
cd $dir
rm -rf *.o *.a
ifort *.f -r8 -c -convert big_endian
ar -r lib${dir}.a *.o
ranlib lib${dir}.a
mv  lib${dir}.a ../
rm -rf *.o
cd ..
done

ifort aux.f -r8 -c -convert big_endian 
ar -r libaux.a *.o
ranlib libaux.a
rm -rf *.o
