      program BARNES
      parameter(M=73,N=37,L=37)
      real u(M,N),v(M,L)

      open(21,file='../fort.16',form='unformatted')

	

         read(21)u
	 do i=1,M
	 do j=1,L
	  v(i,j)=u(i,j)*24.
	 end do
	 end do

         write(30)((v(i,j),i=1,M),j=1,L)

	end
