      program BARNES
      parameter(M=73,N=37,L=15)
      real u(M,N),v(M,L)

      open(21,file='../fort.35',form='unformatted')

	

	do kk=1,10
         read(21)u
	 do i=1,M
	 do j=1,L
	  v(i,j)=u(i,j)
	 end do
	 end do

         write(30)((v(i,j),i=1,M),j=1,L)

	end do

	end
