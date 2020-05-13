	parameter(NLON=96,NLAT=48,NT=90,NT1=NT,nm=30,ns=nm,
     *    cent=10.)
	real st(NLON,NLAT),fll(73,37),x11(72,15),
     *    x(73,15),compp(73,15,50),compn(73,15,50)
	real pot(NLON,NLAT),tt(73,15,nm),cp(73,15),cn(73,15)
	real phm(NLON,NLAT),sa(nm),
     *    phmon(NLON,NLAT,ns),sig(73,15)


      open(15,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/SLP_daily.dat'
     *    ,form='unformatted',status='old')
      open(26,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/SLP_daily73.dat'
     *    ,form='unformatted')



        do  kk=1,14400
	   print*,kk
        read(15)pot

cc Convert to lat-lon grid
        call gautogrid(pot,fll)
 	write(26)fll
	 
	end do
 
        stop
        end

#include "gausstogrid96X48.f"


