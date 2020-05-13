CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=M+1,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N),fll0(M1,N)
        real fll1(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(M1,N)


CC---

        OPEN(10,FILE='SLP.dat',  
     &form='UNFORMATTED',status='old')
CC----------------------

        read(10)pot
        call gautogrid(pot,fll0)

C
	do kk=1,20
	read(10)pot
        call gautogrid(pot,fll)
	do i=1,73
	do j=1,37
 	  var(i,j)=(fll(i,j)-fll0(i,j))*100.
	end do
	end do
        write(20)var

	enddo




	print*, 'Good Job!'




      STOP
      END


#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
