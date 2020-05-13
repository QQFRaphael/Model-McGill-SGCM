	parameter(nm=1,M=72,M1=M+1,N=37,Nrun=30)
	parameter(NLON=64,NLAT=32)
	real st(NLON,NLAT),fll(M1,N),tt(M1,N),ts(M1,N,nm)
	real pot(M1,N),y1(nm),y2(nm),ss(NLON,NLAT)

        open(21, file='../SF.dat',form='unformatted',status='old')

        do i=1,M1
        do j=1,N
         tt(i,j)=0.
        end do
        end do

	do k=1,2
          read(21)st
cc Convert to lat-lon grid
          call gautogrid(st,fll)
	do i=1,M1
	do j=1,N
	  tt(i,j)=tt(i,j)+fll(i,j)/real(2)
	end do
	end do
	end do

	write(16) tt


          read(21)
          read(21)
          read(21)

	



        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
