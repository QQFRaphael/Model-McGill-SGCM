      program BARNES
      parameter(M=73,N=37)
      real u(M,N),v(M,N)

      open(21,file='fort.50'
     &          ,form='unformatted')


	do i=1,16
         read(21)
	end do

	do i=1,M
	do j=1,N
	v(i,j)=0
	end do
	end do




         do ii=1,10
         read(21)u
         write(30)((u(i,j),i=1,73),j=1,15)
         read(21)
         end do


        end
