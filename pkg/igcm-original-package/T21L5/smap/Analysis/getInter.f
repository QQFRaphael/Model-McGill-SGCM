	parameter(nm=51*3,ns=nm,M=72,M1=M+1,N=37,Nrun=20)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real tt(M1,N),ss(M1,N)
	character*1 enum1
        character*2 enum2


 	open(17,file='Z500.dat'
     &		,form='unformatted')
cc begin loop through 5 runs*********************************
	do ie=1,Nrun
	if(ie.lt.10)then
	  write(enum1,'(i1)')ie
	   open(21,file='/pike/jiaxj/igcm/Result/EXPInt/exp'//enum1//
     &    '/Enssum_Z.dat',form='unformatted',status='old')
	  print*,'exp'//enum1//'/Enssum_Z.dat'

	else if(ie.ge.10)then
	  write(enum2,'(i2)')ie
 	  open(21, file='/pike/jiaxj/igcm/Result/EXPInt/exp'//enum2//
     &    '/Enssum_Z.dat',form='unformatted',status='old')
	  print*,'exp'//enum2//'Enssum_Z.dat'
	end if


	do k=1,nm
          read(21)st
          read(17)ss
cc Convert to lat-lon grid
          call gautogrid(st,fll)
	do i=1,M1
	do j=1,N
	 tt(i,j)=fll(i,j)-ss(i,j)
	end do
	end do
          write(18)tt
        end do
	rewind(17)

	end do

        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
