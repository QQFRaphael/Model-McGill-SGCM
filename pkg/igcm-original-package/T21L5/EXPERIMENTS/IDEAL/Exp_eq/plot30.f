CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(M1,N)


CC---

        OPEN(10,FILE='Clim/Enssum_SLP.dat',  
     &form='UNFORMATTED',status='old')
        OPEN(11,
     &  FILE='Enssum_SLP.dat', 
c    &  FILE='eq+25-25+asresp/Enssum_SLP.dat', 
c    &  FILE='north25/Enssum_SLP.dat', 
c    &  FILE='as-resp/Enssum_SLP.dat', 
     &form='UNFORMATTED',status='old')
CC----------------------

	do kkk=1,90
        read(10)
        read(11)
	end do

        do 3 j=1,37
        do 3 i=1,73
          var(i,j)=0.
3       continue


	do kk=1,30
        read(10)pot
        read(11)st
        do i=1,NLON
        do j=1,NLAT
         tt(i,j)=st(i,j)-pot(i,j)
        end do
        end do

cc Convert to lat-lon grid
        call gautogrid(tt,fll)

        do 1 j=1,37
        do 1 i=1,73
          var(i,j)=fll(i,j)+var(i,j)
1       continue
	end do


        do 2 j=1,37
        do 2 i=1,73
          var(i,j)=var(i,j)/real(30)
2       continue

        write(20)var




	print*, 'Good Job!'




      STOP
      END


#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
