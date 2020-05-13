        Parameter (nm=1,mk=12,nday=182,nyear=51)
	Parameter (mmg=72,M=73,N=15)
	real u1(mmg,N),v1(mmg,N),z1(M,37),zs(mmg,N)
        real cosf(N),sin2f(N)
	real ffi(15)
        real us(mmg,N),vs(mmg,N)
CCCCCCCCCCCCCCCCCC



         ae=6370949.0
         pi=4.0*ATAN(1.0)
         dfi=5.0*pi/180.
	 omiga=7.292E-5
	 g=9.81
		
CCCCCCCCCCCCCCCCCCCCCCC
CC get cosf 

         do j=1,N
          fi=90.-float(j-1)*5.0
          fain=fi*pi/180.
          cosf(j)=cos(fain)
          ffi(j)=2.0*omiga*sin(fain)
         end do
	  cosf(1)=(cosf(2)+cosf(3))/2.
	  ffi(1)=(ffi(2)+ffi(3))/2.


CCCCCCCCCCCCCCC


      open (13,file='Z500.dat'
     *     ,form='unformatted')

      open (14,file='windu.dat'
     *     ,form='unformatted')
      open (15,file='windv.dat'
     *     ,form='unformatted')



CCCCCCCCCCCCCCCCCCCCCCC
CCCC Get Zlave, Ulave, Vlave

	do kkk=1,120
 
         read(13)z1
         do j=1,N
         do i=1,mmg
          zs(i,j)=z1(i,j)
         end do
         end do
	

         call getygrad(zs,u1)
         call getxgrad(zs,v1)

         do j=1,N
         do i=1,mmg
          us(i,j)=-u1(i,j)*g/ffi(j)
          vs(i,j)=v1(i,j)*g/ffi(j)
         end do
         end do

	 write(14)us 
	 write(15)vs 


	end do



CCCCCCCCCCC

	 stop 
	 end 






#include "/zemo2/jiaxj/SUBROUTINES/getxgrad.h"
#include "/zemo2/jiaxj/SUBROUTINES/getygrad.h"
