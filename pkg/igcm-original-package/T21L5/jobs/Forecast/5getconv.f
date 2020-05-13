	Parameter (mmg=73,nng=37,nnl=37)
	real ssp(mmg,nnl,5000)
	real t1(mmg,nnl),t2(mmg,nnl),t3(mmg,nng),t4(mmg,nnl),t5(mmg,nnl)
	real wwp(mmg,nnl)
	real y1(5000)
	character*100 outfile
        character*3 cy
	outfile='/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData/AONAO/Prediction/NewdampData/Z73.dat'
CC---
        call getarg(1,cy)
cc convert from character to integer--
        read(cy,'(i3)')mp


      open (13,file=outfile, form='unformatted')


        OPEN(14,FILE=
     & '/zemo2/jiaxj/Internal/Prediction/NewdampData/fort.35')

        do kk=1,mp
        read(14,*) nn
        end do
        mm=nn-8
        print*, mm

          do nn=1,mm*5
           read(13)
          end do

	  do k=1,5
	   read(13) t3
	   write(51)t3
          end do


	 stop 
	 end 






