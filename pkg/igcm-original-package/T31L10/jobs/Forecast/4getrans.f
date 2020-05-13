	parameter(M=72,M1=M+1,N=37,Nrun=1,nlen=1,nm=5*nlen)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M1,N),ss(M1,N)
	character*1 enum1
        character*2 enum2


        open(21, 
     & file='Z.dat',form='unformatted',status='old')



	  do kk=1,nm
	   print*, kk
          read(21)st
          call gautogrid(st,fll)
          write(41)fll
	  end do


        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
