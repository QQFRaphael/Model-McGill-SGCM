        parameter (np=80)
        parameter ( M=73, N=37,nday=180,L=15)
      Real t1(M,N),tm1(M,L,nday),tl1(M,L)
      Real t2(M,N),tm2(M,L,nday),tl2(M,L)
      Real t3(M,N),tm3(M,L,nday),tl3(M,L)
      Real t4(M,N),tm4(M,L,nday),tl4(M,L)
      Real t5(M,N),tm5(M,L,nday),tl5(M,L)
      Real t6(M,N),tm6(M,L,nday),tl6(M,L)
      Real t7(M,N),tm7(M,L,nday),tl7(M,L)
      Real t8(M,N),tm8(M,L,nday),tl8(M,L)
      Real t9(M,N),tm9(M,L,nday),tl9(M,L)
      Real t10(M,N),tm10(M,L,nday),tl10(M,L)



      open (11, 
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z_daily73.dat'
     *,form='unformatted', status='old')

      open (12, file='anomZ.dat',form='unformatted')


CC get climatology for each grid
        do i=1,M
        do j=1,L
        do k=1,nday
         tm1(i,j,k)=0.
         tm2(i,j,k)=0.
         tm3(i,j,k)=0.
         tm4(i,j,k)=0.
         tm5(i,j,k)=0.
         tm6(i,j,k)=0.
         tm7(i,j,k)=0.
         tm8(i,j,k)=0.
         tm9(i,j,k)=0.
         tm10(i,j,k)=0.
        end do
        end do
        end do


        do k1=1,np
        do 61 k2=1,nday

	 read(11)t1
         do j=1,L
         do i=1,M
           tm1(i,j,k2)=tm1(i,j,k2)+t1(i,j)/real(np)
	 end do
	 end do

	 read(11)t2
         do j=1,L
         do i=1,M
           tm2(i,j,k2)=tm2(i,j,k2)+t2(i,j)/real(np)
	 end do
	 end do

	 read(11)t3
         do j=1,L
         do i=1,M
           tm3(i,j,k2)=tm3(i,j,k2)+t3(i,j)/real(np)
	 end do
	 end do

	 read(11)t4
         do j=1,L
         do i=1,M
           tm4(i,j,k2)=tm4(i,j,k2)+t4(i,j)/real(np)
	 end do
	 end do

	 read(11)t5
         do j=1,L
         do i=1,M
           tm5(i,j,k2)=tm5(i,j,k2)+t5(i,j)/real(np)
	 end do
	 end do

	 read(11)t6
         do j=1,L
         do i=1,M
           tm6(i,j,k2)=tm6(i,j,k2)+t6(i,j)/real(np)
	 end do
	 end do

	 read(11)t7
         do j=1,L
         do i=1,M
           tm7(i,j,k2)=tm7(i,j,k2)+t7(i,j)/real(np)
	 end do
	 end do

	 read(11)t8
         do j=1,L
         do i=1,M
           tm8(i,j,k2)=tm8(i,j,k2)+t8(i,j)/real(np)
	 end do
	 end do

	 read(11)t9
         do j=1,L
         do i=1,M
           tm9(i,j,k2)=tm9(i,j,k2)+t9(i,j)/real(np)
	 end do
	 end do

	 read(11)t10
         do j=1,L
         do i=1,M
           tm10(i,j,k2)=tm10(i,j,k2)+t10(i,j)/real(np)
	 end do
	 end do

61      continue
        end do
CC********tm is the climatology mean******
	rewind(11)


        do 66 k1=1,np
        do 65 k2=1,nday
	  read(11)t1
         do i=1,M
         do j=1,L
          tl1(i,j)=t1(i,j)-tm1(i,j,k2)
         end do
         end do
	 write(12) tl1
	  read(11)t2
         do i=1,M
         do j=1,L
          tl2(i,j)=t2(i,j)-tm2(i,j,k2)
         end do
         end do
         write(12) tl2
          read(11)t3
         do i=1,M
         do j=1,L
          tl3(i,j)=t3(i,j)-tm3(i,j,k2)
         end do
         end do
         write(12) tl3
          read(11)t4
         do i=1,M
         do j=1,L
          tl4(i,j)=t4(i,j)-tm4(i,j,k2)
         end do
         end do
         write(12) tl4
          read(11)t5
         do i=1,M
         do j=1,L
          tl5(i,j)=t5(i,j)-tm5(i,j,k2)
         end do
         end do
         write(12) tl5
          read(11)t6
         do i=1,M
         do j=1,L
          tl6(i,j)=t6(i,j)-tm6(i,j,k2)
         end do
         end do
         write(12) tl6
          read(11)t7
         do i=1,M
         do j=1,L
          tl7(i,j)=t7(i,j)-tm7(i,j,k2)
         end do
         end do
         write(12) tl7
          read(11)t8
         do i=1,M
         do j=1,L
          tl8(i,j)=t8(i,j)-tm8(i,j,k2)
         end do
         end do
         write(12) tl8
          read(11)t9
         do i=1,M
         do j=1,L
          tl9(i,j)=t9(i,j)-tm9(i,j,k2)
         end do
         end do
         write(12) tl9
          read(11)t10
         do i=1,M
         do j=1,L
          tl10(i,j)=t10(i,j)-tm10(i,j,k2)
         end do
         end do
         write(12) tl10


65      continue
66      continue



	 stop 
	 end 





