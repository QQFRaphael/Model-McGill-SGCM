	parameter(nm=10,ns=nm,M=72,M1=M+1,N=37,Nrun=20,
     *    cent=10.)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M,N)
	character*1 enum1
        character*2 enum2

	open(17,file='Z500.dat',form='unformatted')

 	  open(21, file='Z500_PERT-E1.dat',
     &    form='unformatted',status='old')

	
	do k=1,nm
          read(21)pot

cc Convert to lat-lon grid
          call gautogrid(pot,fll)
	  write(17)fll
	if(k.eq.nm)print*,fll
	end do

        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
