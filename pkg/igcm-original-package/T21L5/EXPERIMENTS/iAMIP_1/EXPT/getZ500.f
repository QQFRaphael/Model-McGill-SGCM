	parameter(nm=51*6,ns=nm,M=72,M1=M+1,N=37,Nrun=5,
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

	open(17,file='Z500.dat',form='unformatted')
cc begin loop through 5 runs*********************************
	do ie=1,Nrun
	if(ie.lt.10)then
	  write(enum1,'(i1)')ie
	   open(21, file='exp'//enum1//
     &    '/ZT_monthly.dat',form='unformatted',status='old')
	  print*,'exp'//enum1//'_ZT_monthly.dat'

	else if(ie.ge.10)then
	  write(enum2,'(i2)')ie
 	  open(21, file='exp'//enum2//
     &    '/ZT_monthly.dat',form='unformatted',status='old')
	  print*,'exp'//enum2//'_ZT_monthly.dat'
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
cc read Z500
          read(21)pot
cc read MSL PS
	  read(21)
cc 2 month average
c	if(im.ne.1)then
	  do i=1,NLON
          do j=1,NLAT
            st(i,j)=st(i,j)+pot(i,j)/3.
          end do
          end do
c	end if
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
