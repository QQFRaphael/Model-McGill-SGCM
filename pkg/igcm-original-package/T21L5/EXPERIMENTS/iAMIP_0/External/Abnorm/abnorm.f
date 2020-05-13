        parameter(nm=3,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
        parameter(NLON=64,NLAT=32)
	real pot1(NLON,NLAT)
        real pot2(NLON,NLAT)
        real pot3(NLON,NLAT)


        open(21,file='fort.21'
     &    ,form='unformatted',status='old')

        open(22,file='fort.22'
     &    ,form='unformatted',status='old')

        open(23,file='fort.23'
     &    ,form='unformatted')


 	read(21) pot1
	read(22) pot2

          do i=1,NLON
          do j=1,NLAT
            pot3(i,j)=pot1(i,j)-pot2(i,j) 
          end do
          end do

 	write(23)pot3
	print*, pot3	



        stop
        end

