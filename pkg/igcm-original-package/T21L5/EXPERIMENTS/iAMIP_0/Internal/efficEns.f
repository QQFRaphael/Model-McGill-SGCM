        Parameter ( nm=51, ng=36*14, nEmax=10,Nrun=30)
      Real t(72,37),tm(72,15),t1(72,37),t2(72,15,nm),
     &      var(ng,nm*Nrun), cct(ng,ng), e(ng,nEmax),
     &      t3(72,15,nm*Nrun),c(4,nm*Nrun)

      open(15,file=
     * 'Z500.dat',
     &   form='unformatted',status='old')


        rewind (15)

        do k=1,nm
         do i=1,72
         do j=1,15
          t2(i,j,k)=0.
         end do
         end do
        end do

         do it=1,Nrun 
         do k=1,nm
          read(15)t1
          do i=1,72
          do j=1,15
           t2(i,j,k)=t2(i,j,k)+t1(i,j)/30.
          end do
          end do
         end do
         end do
        rewind (15)
CC****t2 is the seasonal mean, k represent the year************

         do it=1,Nrun 
         do k=1,nm
	    l=k+(it-1)*51

          read(15)t1
          do i=1,72
          do j=1,15
           t3(i,j,l)=t1(i,j)-t2(i,j,k)
          end do
          end do
         end do
         end do
        rewind (15)
cc********t3 is the internal part*********************

        do 16 i=1,72
        do 16 j=1,15
16      tm(i,j)=0.

        do 61 k=1,nm*Nrun
         do 12 i=1,72
         do 12 j=1,15
12       tm(i,j)=tm(i,j)+t3(i,j,k)/float(nm*Nrun)
61      continue

CC**********tm is the total average******************


        do 62 k=1,nm*Nrun
         do i=1,72
         do j=1,15
          t(i,j)=t3(i,j,k)
         end do
         end do

	  ii=0
CC 80N-20N
        do 1 j=2,15
         phi=90.-(j-1)*5.
         phi=phi*3.1415926/180.
        do 1 i=1,36
          ii=ii+1
          i5=2*(i-1)+1
         var(ii,k)=(t(i5,j)-tm(i5,j))*sqrt(cos(phi))
1       continue
62      continue

CC**********Obtain the PCs*****************************

c       open  (2, file='../../../Observe/Z500EOF',
c       open  (2, file='../../../Season/Eof/Z500EOF',
 	open  (2, file='Z500EOF',
     *     form='unformatted')

        read (2) e

        do 44 i=1,4
        do 44 n=1,nm*Nrun
        c(i,n)=0.0
        do 45 m=1,ng
45      c(i,n)=c(i,n)-e(m,i)*var(m,n)
44      continue


        do k=1,nm*Nrun
        write(85)c
        end do
      stop
      end


