
        Parameter (nm=182*51,mk=18,nday=182,nyear=51)
        Parameter (mmg=72,M=72,N=17,MM=73,NN=37)
        parameter(NLON=64,NLAT=32)
        real u1(mmg,N),v1(mmg,N),z1(73,N)
        real cosf(N),sin2f(N)
        real ttpu(M,N,mk),ttpv(M,N,mk),ttpz(M,N,mk)
        real ssp1(M,N,5000),ssp2(M,N,5000),ssp3(M,N,5000)
        real y1(nm),za(N),ua(N),va(N)
        real uszs(M,N),vszs(M,N),usvs(M,N),vsvs(M,N)
        real uszs1(M,N),vszs1(M,N),uszs2(M,N),vszs2(M,N)
        real diuszs(M,N),divszs(M,N),flamd(M,N),frefa(M,N)
        real pu(M,N),pv(M,N),pz(M,NN)
        real wws1(M,N),wws2(M,N)
        real tm(mmg,NN),tm0(MM,NN)
        real ztm0(MM,NN),ztm(MM,NN)
        real var(NLON,NLAT), st(NLON,NLAT)



CCCCdefine some constant
CCC
         seita=500./1000.
         ae=6370949.0
         pi=4.0*ATAN(1.0)
         dfi=5.0*pi/180.
	 omiga=7.292E-5
	 g=9.81
		
CCCCCCCCCCCCCCCCCCCCCCC
CC get seita*cosf and 2*omiga*sin2f

         do j=1,N
          fi=90.-float(j-1)*5.0
          fain=fi*pi/180.
          cosf(j)=cos(fain)
          sin2f(j)=1./real(sin(2*fain)*2.*omiga)
         end do
	  cosf(1)=0.
	  sin2f(1)=0.


CCCCCCCCCCCCCCC


      open (41, file='Zave.dat',form='unformatted')
      open (42, file='Zanom.dat',form='unformatted')
      open (11, file=
     &'/zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/IDEAL/Exp_eq/'//
     &'Clim/Enssum_Z.dat',form='unformatted', status='old')
      open (12, file='fort.20',form='unformatted')



        do kkk=1,30
        read(11)
        end do

        do 3 j=1,NLAT
        do 3 i=1,NLON
          var(i,j)=0.
3       continue

        do kk=1,90
        read(11)st
        do i=1,NLON
        do j=1,NLAT
         var(i,j)=var(i,j)+st(i,j)/real(90)
        end do
        end do
        end do
	call gautogrid(var,tm0)
        do j=1,37
        do i=1,72
          tm(i,j)=tm0(i,j)
	end do 
	end do 
           write(41) tm


	   read(12) ztm
	   do i=1,M
	   do j=1,NN
	     pz(i,j)=ztm(i,j)
	   end do
	   end do
           write(42) pz



	 stop 
	 end 

#include "/zemo2/jiaxj/SUBROUTINES/gausstogrid.f"
