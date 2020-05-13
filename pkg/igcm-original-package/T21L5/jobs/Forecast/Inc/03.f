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
     * file='fort.41', form='unformatted')
      open (13,
     * file='fort.51', form='unformatted')


	   read(12) t1
	   read(13) t2

	   do i=1,mmg
	   do j=1,nng
	     t3(i,j)=t2(i,j)-t1(i,j)
	   end do
	   end do
	     write(33) t3

CCCCCCCCCCC

	 stop 
	 end 






