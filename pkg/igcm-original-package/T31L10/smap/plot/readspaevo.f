      program BARNES
      parameter(M=73,N=37)
      real u(M,N),v(M,N)

      open(21,file=
     &'../Z500_73.dat'
     &		,form='unformatted')


         read(21)u
	 print*, u
         write(30)((u(i,j),i=1,73),j=1,15)

	end
