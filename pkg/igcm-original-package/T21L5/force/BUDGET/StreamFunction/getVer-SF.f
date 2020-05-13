	parameter(nm=51,M=72,M1=M+1,N=37,Nrun=30)
	parameter(NLON=64,NLAT=32)
	real st(NLON,NLAT),fll(M1,N),tt(M1,N),ts(M1,N,nm)
	real pot(M1,N),y1(nm),y2(nm),ss(NLON,NLAT)

        open(21, file='SF.dat',form='unformatted',status='old')

	do iy=1,51
        do i=1,M1
        do j=1,N
         tt(i,j)=0.
        end do
        end do

	do k=1,5
          read(21)st
cc Convert to lat-lon grid
          call gautogrid(st,fll)
	do i=1,M1
	do j=1,N
	  tt(i,j)=tt(i,j)+fll(i,j)/real(5)
	end do
	end do
	end do

        do i=1,M1
        do j=1,N
         ts(i,j,iy)=tt(i,j)
        end do
        end do

	end do
	
CCCCCCCCCCCCCC
      open(15,file='/diskc/jiaxj/ncarplot/SVD/fort.31')

        do k=1,nm
         read(15,*) y1(k)
        end do
cc detrend
c       call detrend(y1,nm,a1,b1)
c       do k=1,nm
c         y1(k)=y1(k)-a1*k-b1
c       end do

         s=0.
        do k=1,nm
         s=s+y1(k)/float(nm)
        end do

         std=0.
        do k=1,nm
         std=std+(y1(k)-s)**2/50.
        end do
        std=sqrt(std)
        print*,std

CCC
CCCCCCC
        do i=1,73
        do j=1,37
         do k=1,nm
          y2(k)=ts(i,j,k)
         end do

         call regress(y1,y2,nm,a,b)
         pot(i,j)=a*std
        end do
        end do
        write(16)pot



	print*,'good Job!'
        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"


        subroutine detrend(y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
        do i=1,nm
        x(i)=i
        end do
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.
        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end

CCCCCCCCCCCCCCCC
        subroutine regress(x,y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.

        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end


