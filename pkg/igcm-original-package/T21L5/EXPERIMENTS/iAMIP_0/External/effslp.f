        Parameter ( nm=51, ng=36*14)
      Real t(72,37),tm(72,15),e(ng,15),c(4,nm),
     &      var(ng,nm), cct(ng,ng),fllp(73,37)
	real ttp(72,15,nm),t1(72,37),t2(72,15,nm)

      open (11, file=
     *   'SLP.dat',form='unformatted',
     *         status='old')

        do 16 i=1,72
        do 16 j=1,15
16      tm(i,j)=0.

        do 61 k=1,nm*30
	  read(11)t
         do 12 j=1,15
         do 12 i=1,72
12       tm(i,j)=tm(i,j)+t(i,j)/float(nm*30)
61      continue

	rewind(11)

        do k=1,nm
         do i=1,72
         do j=1,15
          t2(i,j,k)=0.
         end do
         end do
        end do

         do it=1,30
         do k=1,nm
          read(11)t1
          do i=1,72
          do j=1,15
           t2(i,j,k)=t2(i,j,k)+t1(i,j)/30.
          end do
          end do
         end do
         end do


        do 62 k=1,nm
	 do i=1,72
         do j=1,15
          t(i,j)=t2(i,j,k)
         end do
         end do

          ii=0
CC 20N-80
        do 1 j=2,15
         phi=90.-(j-1)*5.
         phi=phi*3.1415926/180.
        do 1 i=1,36
          ii=ii+1
          i5=2*(i-1)+1
         var(ii,k)=(t(i5,j)-tm(i5,j))*sqrt(cos(phi))
1       continue
62      continue

                write(6,*) 'total gridpoints ',ii

	open  (2, file='SLPEOF',
     *     form='unformatted')

 	read(2)e 

	do 44 i=1,4
	do 44 n=1,nm
	c(i,n)=0.0
	do 45 m=1,ng
45	c(i,n)=c(i,n)-e(m,i)*var(m,n)
44	continue
c	open(3,file='PCZ500Ens',form='unformatted')
c	write(3)c
	

	do k=1,nm
        write(88,*)k+47,c(1,k)
        end do
      stop
      end

