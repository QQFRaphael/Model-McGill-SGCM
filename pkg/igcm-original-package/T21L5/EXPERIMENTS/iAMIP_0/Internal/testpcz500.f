        Parameter ( nm=51, ng=36*14,Nrun=30,nEmax=10)
      Real t(72,37),tm(72,15),e(ng,10),c(4,nm),
     &  var(ng,nm*Nrun), cct(ng,ng),fllp(73,37),t3(72,15,nm*Nrun)
	real ttp(72,15,nm),t1(72,37),t2(72,15,nm)

      open (11, file=
     *   'Z500.dat',form='unformatted',
     *         status='old')

	rewind(11)

        do 16 i=1,72
        do 16 j=1,15
16      tm(i,j)=0.

        do 61 k=1,nm*Nrun
	  read(11)t
         do 12 j=1,15
         do 12 i=1,72
12       tm(i,j)=tm(i,j)+t(i,j)/float(nm*Nrun)
61      continue
	rewind(11)
C*********tm is the climatology**********

         do it=1,Nrun
         do k=1,nm
            l=k+(it-1)*51
          read(11)t1
          do i=1,72
          do j=1,15
           t3(i,j,l)=t1(i,j)
          end do
          end do
         end do
         end do
        rewind (11)
CC****t3 is the observed fields,l=nm*Nrun ************


        do 62 k=1,nm*Nrun
	 do i=1,72
         do j=1,15
          t(i,j)=t3(i,j,k)
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
        print*, ii,k

	open  (2, file='Z500EOF',
     *     form='unformatted')

	read (2) e

	do 44 i=1,4
	do 44 n=1,nm*Nrun
	c(i,n)=0.0
	do 45 m=1,ng
45	c(i,n)=c(i,n)-e(m,i)*var(m,n)
44	continue
c	open(3,file='PCsZ500Ens',form='unformatted')
c	write(3)c
	

	do k=1,nm*Nrun
c       write(88,*)k+47,c(1,k)
        write(87,*)c(1,k)
	write(88,*)c(2,k)
        end do
      stop
      end

