	parameter(nm=51*3,ns=nm,M=72,M1=M+1,N=37,Nrun=1)
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

 	open(17,file='SLP.dat',form='unformatted')
        open(21, file='/mnt/climate/data/loach/jiaxj/Data/SGCM/'//
     &  'OutData/51Modelforcing/exp1/SLP_monthly.dat',
     &    form='unformatted',status='old')

	
cc begin loop through 51 winters/////////////////////////
cc initialize 51 winter average for each run

	do k=1,nm

          read(21)st
cc Convert to lat-lon grid
          call gautogrid(st,fll)
	do i=1,M1
	do j=1,N
	 tt(i,j,k)=tt(i,j,k)+fll(i,j)/real(Nrun)
	end do
	end do
	
CC end loop of the 51*6 fields
	end do




        do k=1,nm
        do i=1,M1
        do j=1,N
          fllm(i,j)=tt(i,j,k)
        end do
        end do
          write(17)fllm
        end do




	print*,'good Job!'
        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
