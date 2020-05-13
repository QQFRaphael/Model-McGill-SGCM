CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),qot(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(M1,N)


CC---

        OPEN(10,
     &  FILE='/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/'//
     &  'Exp_eq/Clim/'//
     &  'Enssum_SLP.dat', form='UNFORMATTED',status='old')
        OPEN(11,FILE=
     &  '/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData'//
     &  '/IDEAL/145W/OUTpred_monthly_SLP.dat', 
     &  form='UNFORMATTED',status='old')
CC----------------------

	do k=1,30
	read(10)
	end do

        do i=1,NLON
        do j=1,NLAT
         st(i,j)=0.
        end do
        end do

	
	do k=1,90
	read(10) pot
        do i=1,NLON
        do j=1,NLAT
         st(i,j)=st(i,j)+pot(i,j)/real(90)
        end do
        end do
        end do
	
CCCCCCCCCC

	do kk=1,50	
        read(11)qot

        do i=1,NLON
        do j=1,NLAT
         tt(i,j)=qot(i,j)-st(i,j)
        end do
        end do

cc Convert to lat-lon grid
        call gautogrid(tt,fll)

        write(21)fll
	
	end do


      STOP
      END


#include "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"
