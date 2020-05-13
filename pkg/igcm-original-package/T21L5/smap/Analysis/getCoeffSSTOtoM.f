        Parameter ( nm=51, ng=36*14,nx=73,ny=37)
        Real t(nx,ny),tm(nx,ny),e(ng,10),c(4,nm),
     &      var(ng,nm), cct(ng,ng)
	real ttp(nx,ny,nm),y1(nm),y3(nm)

      open (11, file=
     * 'Z500.dat',
     *  status='old',form='unformatted')

        do 16 i=1,nx
        do 16 j=1,ny
16      tm(i,j)=0.

        do 61 k=1,nm
	  read(11)t
	
         do 12 j=1,ny
         do 12 i=1,nx
         ttp(i,j,k)=t(i,j)
12       tm(i,j)=tm(i,j)+t(i,j)/float(nm)
61      continue

        do 62 k=1,nm
          ii=0
CC 20N-80
        do 1 j=1,17
        do 1 i=1,31
          ii=ii+1
         var(ii,k)=ttp(i,j,k)-tm(i,j)
1       continue
62      continue

        write(6,*) 'total gridpoints ',ii

	open  (2, file='EOF',
     *     form='unformatted')

	read(2)e

	do 44 i=1,4
	do 44 n=1,nm
	c(i,n)=0.0
	do 45 m=1,ng
45	c(i,n)=c(i,n)+e(m,i)*var(m,n)
44	continue



c     open(13,file='PCindex')




	do ii=1,nm
 	 write(13,*) c(1,ii)
	end do



      stop
      end

