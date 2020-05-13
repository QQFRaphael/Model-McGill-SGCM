        parameter(nm=3,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
        parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT)



        open(21,file='fort.21'
     &    ,form='unformatted',status='old')

        open(22,file='fort.22'
     &    ,form='unformatted',access='append')

	do i=1,3
 	read(21) pot
 	write(22)pot
	
	end do



        stop
        end

