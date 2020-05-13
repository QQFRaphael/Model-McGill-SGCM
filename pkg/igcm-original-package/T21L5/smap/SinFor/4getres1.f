CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(NLON,NLAT)



CC---

        OPEN(10,
     &  FILE='/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/'//
     &  'Exp_eq/Clim/'//
     &  'Enssum_SLP.dat', form='UNFORMATTED',status='old')


        OPEN(11,FILE=
     &  '/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData'//
     &  '/IDEAL/155E+165W25N/Enssum_SLP.dat', 
     &  form='UNFORMATTED',status='old')
CC----------------------

	do kkk=1,30
        read(10)
        read(11)
	end do

        do 3 j=1,NLAT
        do 3 i=1,NLON
          var(i,j)=0.
3       continue


	do kk=1,90
        read(10)pot
        read(11)st
        do i=1,NLON
        do j=1,NLAT
         var(i,j)=var(i,j)+(st(i,j)-pot(i,j))/real(90)
        end do
        end do
        end do

cc Convert to lat-lon grid
        call gautogrid(var,fll)

        write(20)fll




      STOP
      END


#include "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"
