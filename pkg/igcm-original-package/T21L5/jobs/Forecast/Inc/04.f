        Parameter (mk=12)
	Parameter (mmg=73,nng=37,nnl=37)
	real ssp(mmg,nnl,5000),ttp(mmg,nnl,mk)
	real t1(mmg,nnl),t2(mmg,nnl),t3(mmg,nng),t4(mmg,nnl),t5(mmg,nnl)
	real wwp(mmg,nnl)
	real y1(5000)
         character*3 cy
         call getarg(1,cy)
         read(cy,'(i3)') mp


      open (12,
     * file='../rat.dat', form='unformatted')


	   do k=1,650
	    read(12)bb
	   end do

	     aa=0.
	   do k=1,170
	    read(12)bb
	    aa=aa+bb/real(170)
	   end do
	    print*,aa

CCCCCCCCCCC

	 stop 
	 end 






