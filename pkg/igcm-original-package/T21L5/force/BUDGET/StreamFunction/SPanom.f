	parameter(nm=51,M=72,M1=73,N=37,Nrun=30)
	parameter(NLON=64,NLAT=32)
	real st1(NLON,NLAT),st2(NLON,NLAT),st(NLON,NLAT)
	real stt(NLON,NLAT),st0(NLON,NLAT)
	real pot(M1,N),y1(nm),y2(nm),ss(NLON,NLAT)
	real pot0(M1,N)

        open(20, file='climsf.dat',form='unformatted',status='old')
        open(21, file='SF.dat',form='unformatted',status='old')

	do kk=1,5

        do i=1,NLON
        do j=1,NLAT
	  st(i,j)=0.
	  stt(i,j)=0.
	end do
	end do


c       do k=1,3
c         read(20)st1
c         read(21)st2
c	end do


c       do k=1,2
          read(20)st1
          read(21)st2
        do i=1,NLON
        do j=1,NLAT
	  st(i,j)=st(i,j)+st1(i,j)/real(1)
	  stt(i,j)=stt(i,j)+st2(i,j)/real(1)
	end do
	end do
c	end do



        do i=1,NLON
        do j=1,NLAT
         st0(i,j)=(stt(i,j)-st(i,j))/real(1000000)
        end do
        end do




cc Convert to lat-lon grid
          call gautogrid(st0,pot)

   	write(16)pot


	end do


	print*,'good Job!'
        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"


