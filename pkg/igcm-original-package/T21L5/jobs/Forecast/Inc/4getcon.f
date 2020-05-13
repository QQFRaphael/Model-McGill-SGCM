        Parameter (mk=4)
	Parameter (mmg=73,nng=37,nnl=37)
	real ssp(mmg,nnl,5000),ttp(mmg,nnl,mk)
	real t1(mmg,nnl),t2(mmg,nnl),t3(mmg,nng),t4(mmg,nnl),t5(mmg,nnl)
	real wwp(mmg,nnl)
	real y1(5000)
         character*3 cy
         call getarg(1,cy)
         read(cy,'(i3)') mp


      open (13,
     * file='/zemo2/jiaxj/Internal/Prediction/Data/Z_daily73.dat', 
     * form='unformatted')


	  do kkk=1,mp
	    print*, kkk
          do nn=1,(21+30*(kkk-1))*5
           read(13) 
          end do
	  do k=1,5
	   read(13) t3
	   write(51)t3
          end do

	 do kk=1,mk
	  do k=1,20
	   read(13)
	  end do
	  do k=1,5
	   read(13) t3
	   write(51)t3
          end do
	 end do
	  rewind(13)

	 end do
CCCCCCCCCCC

	 stop 
	 end 






