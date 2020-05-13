        parameter(ng=36*14, nMax=4)
        real x(37,14),evt(ng,nMax),spa(ng)
        real time1(ng),time2(ng)



         open(17,file='../roteof',form='unformatted')
	 read(17) evt

         ii=0
         do k=1,ng
         ii=ii+1
         time1(ii)=-evt(ii,1)
	 write(30,*) time1(ii)
	 enddo

         open(18,file='../EOF',form='unformatted')
	 read(18) evt

         ii=0
         do k=1,ng
         ii=ii+1
         time2(ii)=-evt(ii,2)
	 write(31,*) time2(ii)
	 enddo

	


	end
