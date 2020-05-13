(Reference: /diska/hlin/igcm/force)


51x90 day data( from 1948,1949)

1. UVTP4849after.dat is 51 DJF season daily Gaussian grid (128x64) data at 5 sigma levels
        generated on tropo:/diskc/jiaxj/force/readuvzt.sh( use readuvzt.f)

2. specan is done on zemo, which convert UVTP4849after.dat
	to spectral format, UVTP4849after.dat.spec
     ******this work has been done by /zemo2/jiaxj/force/specan.sh( use specan.f)
     ******As a matter of fact all are spectral data is for Z,D,T,SP

2a. Doing divergence correction on zemo-- Make sure there is no time averaged divergence in a 
    fixed column---
    generate new spectral data set: /zemo2/jiaxj/force_diag/UVTP4849after.dat.spec_corrected
    *******this work has been done by zemo:/zemo2/jiaxj/force/divCOR.ex( use specan2.f addCOR.f)
    *****( this data file will be used to run the model to integrate in the future )
 
 
    *******************************************************************************************
    **********the following steps will calculate the force fields for the 51 years*************
    *******************************************************************************************


3. On zemo, In /zemo2/jiaxj/igcm/jobs/T21L5.48/, run MODEL.job for one time step reading
   initial conditions from /zemo2/jiaxj/force/UVTP4849after.dat.spec_corrected, the state 
   at time step one are stored at /zemo2/jiaxj/igcm/EXPR48/restart.12
   (UVTP4849after.dat.spec_corrected, this file will be used in running model in the future)

******for UVTP4849after.dat.spec_corrected and restart.12 are the initial and forcest(one step) 
      data for each year, totally we have 90*51 days.To calculate force field, we need to use 
      average 90 days data to get the initial and forcast data for each year.  


4. Calculating forcing: get tendency from restart.12 and UVTP4849after.dat.spec_corrected 
   use zemo:/zemo2/jiaxj/force/getforce.sh(use getforce.f)
   get /zemo2/jiaxj/force/FORCE_51DJF.dat_crtd 
   ****** for each year, we could get a force field, totally we could get 51 force fields*****
   ****** this force field is in the form of spectrum form, we need to change it to grid form******

C  5 Transfor spectrum form forcing to grid form forcing
C    use zemo:getforce_grid.sh(use getforce_grid.f)




6. Plot forcing
   First need to convert it to grid points use /D1/hlin/igcm/data/getforce_grid.f
   Then plotforce.f

7. In /D1/hlin/igcm/smap/T21L5, SMAP diagnostic package is used to
   get pressure level U,V,T,Z,SP for STEP1 and STEP0(data), the difference between
   these two states is forcing (unit per hour), that is plotted in /diska/hlin/igcm/force/plot
