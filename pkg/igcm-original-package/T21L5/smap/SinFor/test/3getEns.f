CC now changed to get 90 day average---

      PROGRAM CHECKHST
        parameter(nm=120,ns=nm,M=72,M1=73,N=37,Nrun=9)
        parameter(NLON=64,NLAT=32)
        real var(NLON,NLAT),pot(NLON,NLAT)


CC---

        OPEN(10,FILE='/mnt/climate/data/loach/jiaxj/Data/'//
     &  'SGCM/OutData/IDEAL/145W/OUTpred_monthly_SLP.dat',
     &   form='UNFORMATTED',status='old')
        OPEN(11,FILE='/mnt/climate/data/loach/jiaxj/Data/'//
     &  'SGCM/OutData/IDEAL/145W/Enssum_monthly_SLP.dat',
     &   form='UNFORMATTED')
CC----------------------


        do j=1,NLAT
        do i=1,NLON
          var(i,j)=0.
	end do
	end do

        do kk=1,50
        read(10)pot
        do i=1,NLON
        do j=1,NLAT
         var(i,j)=var(i,j)+pot(i,j)/real(50)
        end do
        end do
        end do

	write(11) var

	stop
	end


