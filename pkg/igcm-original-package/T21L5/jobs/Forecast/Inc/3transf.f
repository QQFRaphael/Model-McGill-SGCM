	parameter(M=72,M1=M+1,N=37,Nrun=40,nlen=1,nm=nlen*5*Nrun)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M1,N),ss(M1,N)
	character*1 enum1
        character*2 enum2

         character*3 cy
         call getarg(1,cy)
         read(cy,'(i3)') mp
	  mm=mp*5*nlen


        open(21, 
     & file='Z.dat',form='unformatted',status='old')



	  do kk=1,mm
	   print*, kk
          read(21)st
          call gautogrid(st,fll)
          write(41)fll
	  end do


        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
