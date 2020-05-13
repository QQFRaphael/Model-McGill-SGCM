        Parameter (nm=1)
        parameter ( M=73, N=15)
	Parameter (mmg=72,nng=37,nnl=15)
	real u1(mmg,nnl),v1(mmg,nnl)
	real cosf(nnl),sin2f(nnl)
        Real tmz(M,N,nm)
        real ttpz(M,N),trz(M,37),tsz(M,N)
        Real tu(mmg,N),tmu(M,N,nm)
        real ttpu(M,N),tsu(M,N)
        Real tv(mmg,N),tmv(M,N,nm)
        real ttpv(M,N),tsv(M,N)
	real uszs(M,N),vszs(M,N),usvs(M,N),vsvs(M,N)
	real divszs(M,N),diuszs(M,N),flamd(M,N),frefa(M,N)
        real wws1(M,N),wws2(M,N),uszs1(M,N),vszs1(M,N)
	real uszs2(M,N),vszs2(M,N)


CCCCdefine some constant
CCC
 	 seita=500./1000.	
         ae=6370949.0
         pi=4.0*ATAN(1.0)
	 dfi=5.0*pi/180.



CCCCCCCCCCCCCCCCCCCCCCC
CC get seita*cosf and 2*omiga*sin2f

         do j=1,nnl
          fi=90.-float(j-1)*5.0
          fain=fi*pi/180.
          cosf(j)=seita*cos(fain)
 	  sin2f(j)=sin(2*fain)*2.*7.292E-5
         end do



      open (11,file='windu.dat'
     *     ,form='unformatted')
      open (12,file='windv.dat'
     *     ,form='unformatted')
      open (13,file='Z500.dat'
     *     ,form='unformatted')

CCCCCCCCCCCCCCCCCCC
CCC 
	do kkk=1,120

         read(13)trz
         do j=1,N
         do i=1,M
          ttpz(i,j)=trz(i,j)
	 end do
	 end do
         write(42)ttpz


         read(11)tu
         read(12)tv
         do j=1,N
         do i=1,mmg
          ttpu(i,j)=tu(i,j)
          ttpv(i,j)=tv(i,j)
         end do
          ttpu(73,j)=ttpu(1,j)
          ttpv(73,j)=ttpv(1,j)
         end do



         do j=1,N
         do i=1,M
          uszs(i,j)=ttpu(i,j)*ttpz(i,j)
          vszs(i,j)=ttpv(i,j)*ttpz(i,j)
          usvs(i,j)=ttpu(i,j)*ttpv(i,j)
          vsvs(i,j)=ttpv(i,j)*ttpv(i,j)
         end do
         end do


         do j=1,N
         do i=1,M
          uszs1(i,j)=uszs(i,j)*g
          vszs1(i,j)=vszs(i,j)*g
         end do
         end do

        call getxgrad(vszs1,vszs2)
        call getxgrad(uszs1,uszs2)


         do j=1,N
         do i=1,M
          divszs(i,j)=vszs2(i,j)
          diuszs(i,j)=uszs2(i,j)
         end do
         end do


           do j=1,N
           do i=1,M
            flamd(i,j)=seita*cosf(j)*
     &          (vsvs(i,j)-sin2f(j)*divszs(i,j))
            frefa(i,j)=seita*cosf(j)*
     &          (-usvs(i,j)+sin2f(j)*diuszs(i,j))
           end do
           end do


           write(41)flamd,frefa






	end do





CCCCCCCCCCC

	 stop 
	 end 






#include "/zemo2/jiaxj/SUBROUTINES/getxgrad.h"
