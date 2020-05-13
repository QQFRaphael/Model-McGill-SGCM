CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=M+1,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(M1,N)


CC---

        OPEN(10,FILE='SLP.dat',  
     &form='UNFORMATTED',status='old')
        OPEN(20,FILE='season.dat',  
     &form='UNFORMATTED')
CC----------------------


	do kkk=1,100
	  print*, kkk

        do mm=1,62
         read(10)
        end do

        do 3 j=1,37
        do 3 i=1,73
          var(i,j)=0.
3       continue

	do kk=1,120
        read(10)pot
cc Convert to lat-lon grid
        call gautogrid(pot,fll)
        do j=1,37
        do i=1,73
          var(i,j)=fll(i,j)+var(i,j)/real(120)
	end do
	end do
	end do
        write(20)var


	end do




      STOP
      END


#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
