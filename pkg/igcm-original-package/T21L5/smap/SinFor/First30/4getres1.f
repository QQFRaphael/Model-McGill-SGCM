CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(M1,N)


CC---

        OPEN(10,FILE='SLP.dat',form='UNFORMATTED',status='old')
        OPEN(11,FILE='SLP_clim.dat',form='UNFORMATTED',status='old')
CC----------------------

	do kk=1,14
        read(10)pot
        read(11)st
        do i=1,NLON
        do j=1,NLAT
         tt(i,j)=st(i,j)-pot(i,j)
        end do
        end do
cc Convert to lat-lon grid
        call gautogrid(tt,fll)
        write(20)fll
	end do




	print*, 'Good Job!'




      STOP
      END


#include "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"
