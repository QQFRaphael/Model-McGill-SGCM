      program BARNES
      parameter(M=73,N=37)
      real u(M,N),v(M,N)

      open(21,
     &file='/zemo2/jiaxj/force/BUDGET/fort.16', form='unformatted')

	


         read(21)u
	 do i=1,73
	 do j=1,37
	  v(i,j)=u(i,j)*24.
	 end do
	 end do



         write(40)((v(i,j),i=1,73),j=1,37)

	


	end
