        parameter(nm=3,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
        parameter(NLON=64,NLAT=32)

        character*15 outfile, infile
        character year(51)
        data year/'1','2','3','4','5','6','7','8',
     & '9','10','11','12','13','14','15','16','17','18','19',
     & '20','21','22','23','24','25','26','27','28','29','30',
     & '31','32','33','34','35','36','37','38','39','40','41',
     & '42','43','44','45','46','47','48','49','50','51'/

	do iyr=1,51

           open(21,file='exp'//year(iyr)//'/ZT_monthly.dat'
     &    ,form='unformatted',status='old')

	print*, '../exp'//year(iyr)//'/ZT_monthly.dat'


        outfile='year'//year(iyr)//'.dat'
      OPEN(16,FILE=
     &  outfile,form='UNFORMATTED')

	print*,'year'//year(iyr)//'.dat'


	end do	

        stop
        end

