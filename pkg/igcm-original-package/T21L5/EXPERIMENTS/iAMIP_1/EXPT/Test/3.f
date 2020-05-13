	parameter(nm=51,ns=nm,M=72,M1=M+1,N=37,Nrun=1,
     *    cent=10.)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M,N)
	character*1 enum1
        character*2 enum2

	  open(21, file='../exp3/Z_monthly.dat',
     &    form='unformatted',status='old')

 	do kk=1,300
 	  read(21)st
 	end do

	 do k=1,6
	  read(21)pot
          call gautogrid(pot,fll)
	  write(33)fll
	 end do

        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
