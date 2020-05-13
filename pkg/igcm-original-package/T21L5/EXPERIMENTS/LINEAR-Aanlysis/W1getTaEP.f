        Parameter (nm=182*51,mk=18,nday=182,nyear=51)
	Parameter (mmg=72,M=72,N=15,MM=73,NN=37)
	real u1(mmg,N),v1(mmg,N),z1(73,N)
        real cosf(N),sin2f(N)
	real ttpu(M,N,mk),ttpv(M,N,mk),ttpz(M,N,mk)
	real ssp1(M,N,5000),ssp2(M,N,5000),ssp3(M,N,5000)
	real y1(nm),za(N),ua(N),va(N)
        real uszs(M,N),vszs(M,N),usvs(M,N),vsvs(M,N)
        real uszs1(M,N),vszs1(M,N),uszs2(M,N),vszs2(M,N)
	real diuszs(M,N),divszs(M,N),flamd(M,N),frefa(M,N)
	real pu(M,N),pv(M,N),pz(M,N)
        real wws1(M,N),wws2(M,N)	
	real tm(mmg,N),t(144,73)
	real ztm0(MM,NN),ztm(MM,NN)



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
      open (11, file='fort.20',form='unformatted', status='old')



           read(11) ztm0
	   do i=1,M
	   do j=1,N
	     tm(i,j)=ztm0(i,j)
	   end do
	   end do
           write(41) tm


	   read(11)
	   read(11)
	   read(11)
	   read(11)
	   read(11)
	   read(11) ztm
	   do i=1,M
	   do j=1,N
	     pz(i,j)=ztm(i,j)
	   end do
	   end do
           write(42) pz



	 stop 
	 end 


