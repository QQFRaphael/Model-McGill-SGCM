CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
        real x(M1,N),fllm(M1,N),tt(NLON,NLAT)
        real t1(M1,N,nm),t2(M1,N),var(64,32)


CC---

        OPEN(10,
     &  FILE='/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/'//
     &  'Exp_eq/Clim/'//
     &  'Enssum_SLP.dat', form='UNFORMATTED',status='old')


	do nnn=1,4	

        do j=1,32
        do i=1,64
          var(i,j)=0.
	end do
	end do

	do kk=1,30
        read(10)pot
        do 1 j=1,32
        do 1 i=1,64
          var(i,j)=var(i,j)+pot(i,j)
1       continue
	end do

        do 2 j=1,32
        do 2 i=1,64
          var(i,j)=var(i,j)/real(30)
2       continue

        write(20)var

	end do


      END


#include "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"
