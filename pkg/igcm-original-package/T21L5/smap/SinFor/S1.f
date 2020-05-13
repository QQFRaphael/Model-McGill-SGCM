CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll1(M1,N),fll2(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var1(NLON,NLAT),var2(NLON,NLAT)



CC---

        OPEN(10,
     &  FILE='/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/'//
     &  'Exp_eq/Clim/'//
     &  'Enssum_UV.dat', form='UNFORMATTED',status='old')
      open(20,file='UVclim.dat',form='unformatted')



	do kkk=1,30
        read(10)
        read(10)
	end do

        do 3 j=1,NLAT
        do 3 i=1,NLON
          var1(i,j)=0.
          var2(i,j)=0.
3       continue


	do kk=1,90
        read(10)pot
        read(10)st
        do i=1,NLON
        do j=1,NLAT
         var1(i,j)=var1(i,j)+pot(i,j)/real(90)
         var2(i,j)=var2(i,j)+st(i,j)/real(90)
        end do
        end do
        end do

cc Convert to lat-lon grid
        call gautogrid(var1,fll1)
        call gautogrid(var2,fll2)

        write(20)fll1
        write(20)fll2




      STOP
      END


#include "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"
