CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(M1,N)


CC---

        OPEN(11,
     &  FILE='Enssumclim_SLP.dat', 
c    &  FILE='Enssumclim_Z.dat', 
     &form='UNFORMATTED',status='old')
CC----------------------

	do kkk=1,50

        read(11)pot
cc Convert to lat-lon grid
        call gautogrid(pot,fll)
        write(20)fll

	end do

	print*, 'Good Job!'




      STOP
      END


#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
