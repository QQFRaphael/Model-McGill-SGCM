	parameter(nm=51*6,ns=nm,M=72,M1=M+1,N=37,Nrun=1)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M1,N,nm)
	character*1 enum1
        character*2 enum2

cc initialize array to hold ensemble mean climate value
	do k=1,nm
	do i=1,M1
	do j=1,N
	 tt(i,j,k)=0.
	end do
	end do
	end do

 	open(17,file='Z500.dat',form='unformatted')
cc begin loop through 5 runs*********************************
       open(21, file='./exp5/Z_monthly.dat',
     &    form='unformatted',status='old')



	do k=1,nm

          read(21)st
cc Convert to lat-lon grid
          call gautogrid(st,fll)

	  write(17)fll


	
	end do



	print*,'good Job!'
        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
