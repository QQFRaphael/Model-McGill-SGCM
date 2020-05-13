	parameter(nm=51,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M,N)
	character*1 enum1
        character*2 enum2

cc initialize array to hold ensemble mean climate value
	do i=1,M1
	do j=1,N
	 x(i,j)=0.
	end do
	end do

	open(17,file='SLP.dat',form='unformatted')
cc begin loop through 30 runs*********************************
	do ie=1,Nrun
	if(ie.lt.10)then
	  write(enum1,'(i1)')ie
	   open(21, file='../../exp'//enum1//
     &    '/SLP_monthly.dat',form='unformatted',status='old')

	else if(ie.ge.10)then
	  write(enum2,'(i2)')ie
 	  open(21, file='../../exp'//enum2//
     &    '/SLP_monthly.dat',form='unformatted',status='old')
	end if

	
cc begin loop through 51 winters/////////////////////////
cc initialize 51 winter average for each run
	do i=1,M1
	do j=1,N
	 fllm(i,j)=0.
	end do
	end do

	do k=1,nm
cc initialize array to hold 3 month average
	  do i=1,NLON
          do j=1,NLAT
            st(i,j)=0.
          end do
          end do

cc begin loop of 3 months---------
	do im=1,3	
cc read SLP
	  read(21)pot

	  do i=1,NLON
          do j=1,NLAT
            st(i,j)=st(i,j)+pot(i,j)/3.
          end do
          end do
	end do
cc end loop of 3 months----------

cc Convert to lat-lon grid
          call gautogrid(st,fll)
	do i=1,M
	do j=1,N
	 tt(i,j)=fll(i,j)
	end do
	end do
	write(17)tt

	  do i=1,M1
	  do j=1,N
	  fllm(i,j)=fllm(i,j)+fll(i,j)/float(nm)
	  end do
	  end do
	
	end do
cc end loop through 51 winters///////////////////////

	 close(21)

	do i=1,M1
        do j=1,N
         x(i,j)=x(i,j)+fllm(i,j)/float(Nrun)
        end do
        end do

	end do
cc end loop through 30 runs********************************
	write(17)x

        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
