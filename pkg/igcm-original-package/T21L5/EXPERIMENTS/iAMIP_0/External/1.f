        Parameter ( nm=51, ng=36*14)
      Real t(72,37),tm(73,37,10)
	real tt(73,37)

      open (11, file=
     *   'Z500.dat',form='unformatted',
     *         status='old')

        do 16 i=1,73
        do 16 j=1,37
	do 16 k=1,10
16      tm(i,j,k)=0.


	do k=1,10
	  read(11)t

         do 12 j=1,37
         do 12 i=1,72
12       tm(i,j,k)=t(i,j)

	 do 13 j=1,37
	 tm(73,j,k)=t(1,j) 
13	 continue

	end do


	
         do 14 j=1,37
         do 14 i=1,73
	  tt(i,j)=tm(i,j,10)
14	continue

	write(10)tt


      stop
      end

