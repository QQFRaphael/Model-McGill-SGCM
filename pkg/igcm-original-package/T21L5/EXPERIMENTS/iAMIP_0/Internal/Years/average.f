	parameter(nm=90,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N)
	character*1 enum1
        character*2 enum2

cc initialize array to hold ensemble mean climate value
	do i=1,M1
	do j=1,N
	 x(i,j)=0.
	end do
	end do


        open(21,file='fort.21'
     &    ,form='unformatted',status='old')

        open(22,file='fort.22'
     &    ,form='unformatted')




cc initialize array to hold 3 month average
	  do i=1,NLON
          do j=1,NLAT
            st(i,j)=0.
          end do
          end do

        do k=1,nm
	  read(21)pot
	  
	  do i=1,NLON
          do j=1,NLAT
            st(i,j)=st(i,j)+pot(i,j)
          end do
          end do
	end do

	 
cc 30 members average
  
          do i=1,NLON
          do j=1,NLAT
            st(i,j)=st(i,j)/90.
          end do
          end do

	write(22)st

	stop
	end



