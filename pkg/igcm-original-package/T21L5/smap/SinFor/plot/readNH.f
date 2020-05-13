      program BARNES
      parameter(M=73,N=15)
      real u(M,37),v(M,N)

      open(22,file='../fort.20'
     &          ,form='unformatted')


         do ii=1,1
         read(22)u
	  do i=1,M
	  do j=1,N
	   v(i,j)=u(i,j)*1.
	  end do
	  end do
          write(30)((v(i,j),i=1,M),j=1,N)
         end do


        end
