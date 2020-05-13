	parameter(nm=51*3,ns=nm,M=72,M1=M+1,N=37,Nrun=15)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M1,N,nm),ss(M1,N)
	character*1 enum1
        character*2 enum2

cc initialize array to hold ensemble mean climate value
	do k=1,nm
	do i=1,M1
	do j=1,N
	 tt(i,j,k)=0.
	end do
	end do
	end do

 	open(17,file='SLP.dat'
     &		,form='unformatted')
cc begin loop through 5 runs*********************************
	do ie=1,Nrun
	if(ie.lt.10)then
	  write(enum1,'(i1)')ie
	   open(21,file='/pike/jiaxj/igcm/Result/EXPInt/exp'//enum1//
     &    '/SLP_monthly.dat',form='unformatted',status='old')
	  print*,'exp'//enum1//'/Enssum_Z.dat'

	else if(ie.ge.10)then
	  write(enum2,'(i2)')ie
 	  open(21, file='/pike/jiaxj/igcm/Result/EXPInt/exp'//enum2//
     &    '/SLP_monthly.dat',form='unformatted',status='old')
	  print*,'exp'//enum2//'Enssum_Z.dat'
	end if


	
cc begin loop through 51 winters/////////////////////////
cc initialize 51 winter average for each run

	do k=1,nm
          read(21)st
cc Convert to lat-lon grid
          call gautogrid(st,fll)
	do i=1,M1
	do j=1,N
	 tt(i,j,k)=tt(i,j,k)+fll(i,j)/real(Nrun)
	end do
	end do
CC end loop of the 51*6 fields
	end do


	close(21)
CC end loop of the run
	end do


	do ii=1,51

        do k=3*ii-2,3*ii
        do i=1,M1
        do j=1,N
          ss(i,j)=ss(i,j)+tt(i,j,k)/3.
        end do
        end do
	end do
          write(17)ss

        do i=1,M1
        do j=1,N
          ss(i,j)=0.
        end do
        end do

        end do


	print*,'good Job!'
        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
