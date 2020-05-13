#script to take a time series spectral data and apply 
#a divergence correction to it in grid space and then
#output another spectral time series. Note, must first 
#run timeave.f to find time average of spectral record 
#as this is used in calculation. 
#
#first compute the correction term from the time average data
#
echo compiling specan2.f
f77 -O3 -static -r8 -o specan2.out specan2.f -L /usr/people/hall/igcm/lib \
   -l sgifft1 -l sgiutil1 || ABORT f77
ln -s UVTP4849after.dat.spec fort.13
echo running specan2.f
specan2.out
mv fort.9 data.cor
#
#time average the correction and add it to each day
f77 -r8 addCOR.f
echo running addCOR.f
a.out
#
#time average the resulting sequence
#f77 -r8 timeave.f
#a.out
rm -f fort.* 
rm -f data.cor*
rm -f specan2.out*
rm -f a.out

