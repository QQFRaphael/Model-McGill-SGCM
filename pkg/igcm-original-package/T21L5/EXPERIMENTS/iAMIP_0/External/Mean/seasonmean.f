        parameter(nm=3,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
        parameter(NLON=64,NLAT=32)
        real pot(NLON,NLAT),st(NLON,NLAT)


        character*15 outfile, infile
        character*2 dir(30)
        data dir/'01','02','03','04','05','06','07','08',
     & '09','10','11','12','13','14','15','16','17','18','19',
     & '20','21','22','23','24','25','26','27','28','29','30'/


        character*2 year(51)
        data year/'01','02','03','04','05','06','07','08',
     & '09','10','11','12','13','14','15','16','17','18','19',
     & '20','21','22','23','24','25','26','27','28','29','30',
     & '31','32','33','34','35','36','37','38','39','40','41',
     & '42','43','44','45','46','47','48','49','50','51'/



C*************one dir*****************

        open(21,file='fort.21'
     &    ,form='unformatted',status='old')

	do iyr=1,51
        outfile='season'//year(iyr)//'.dat'
      OPEN(16,FILE=
     &  outfile,form='UNFORMATTED')

cc initialize array to hold seasonal average
          do i=1,NLON
          do j=1,NLAT
            st(i,j)=0.
          end do
          end do

        do im=1,3
cc read Z500
          read(21)
          read(21)
          read(21)
cc read MSL PS
          read(21)pot
          read(21)
          read(21)
CC do seasonal average
          do i=1,NLON
          do j=1,NLAT
            st(i,j)=st(i,j)+pot(i,j)/3.
          end do
          end do
        end do
          write(16)st



	end do

C*****************one dir****************




        stop
        end

