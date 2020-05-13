CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=M+1,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var1(M1,N),var2(M1,N)
	real st1(NLON,NLAT),st2(NLON,NLAT),fll1(M1,N),fll2(M1,N)
	real tt1(NLON,NLAT),tt2(NLON,NLAT),var3(M1,N)


CC---

        OPEN(10,FILE='Clim/Enssum_SLP.dat',  
     &form='UNFORMATTED',status='old')
        OPEN(11,FILE='eqresp/Enssum_SLP.dat', 
     &form='UNFORMATTED',status='old')
        OPEN(12,FILE='25resp/Enssum_SLP.dat', 
     &form='UNFORMATTED',status='old')


CC----------------------

	do kkk=1,90
        read(10)pot
        read(11)st1
        read(12)st2
	end do



        do 8 j=1,37
        do 8 i=1,73
          var3(i,j)=0.
8       continue


	do kk=1,30
        read(10)pot
        read(11)st1
        read(12)st2
        do i=1,NLON
        do j=1,NLAT
         tt1(i,j)=st1(i,j)-pot(i,j)
         tt2(i,j)=st2(i,j)-pot(i,j)
        end do
        end do



cc Convert to lat-lon grid
        call gautogrid(tt1,fll1)
        call gautogrid(tt2,fll2)

        do 1 j=1,37
        do 1 i=1,73
          var1(i,j)=fll1(i,j)+var1(i,j)
          var2(i,j)=fll2(i,j)+var2(i,j)
1       continue
	end do


        do 2 j=1,37
        do 2 i=1,73
          var1(i,j)=var1(i,j)/real(30)
          var2(i,j)=var2(i,j)/real(30)
2       continue

        do 3 j=1,37
        do 3 i=1,73
          var3(i,j)=(var1(i,j)+var2(i,j))/2.
3       continue




        write(20)var3




	print*, 'Good Job!'




      STOP
      END


#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
