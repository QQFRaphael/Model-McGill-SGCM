        Parameter ( nm=51*3, ng=36*14)
      Real t(72,37),tm(72,15),e(ng,2),c(4,nm),
     &      var(ng,nm), cct(ng,ng),fllp(73,37)
	real ttp(72,15,nm),t1(72,37),t2(72,15,nm)
	real y1(nm)
	real y2(nm)



      open (11, file=
     *'SLP.dat'
     &,form='unformatted', status='old')


        do 16 i=1,72
        do 16 j=1,15
16      tm(i,j)=0.

        do 61 k=1,nm
	  read(11)t
         do 12 j=1,15
         do 12 i=1,72
	 ttp(i,j,k)=t(i,j)
12       tm(i,j)=tm(i,j)+t(i,j)/float(nm)
61      continue

	rewind(11)
C********tm is the climatology mean******

	do 62 k=1,nm
          ii=0
CC 20N-80
        do 1 j=2,15
         phi=90.-(j-1)*5.
         phi=phi*3.1415926/180.
        do 1 i=1,36
          ii=ii+1
          i5=2*(i-1)+1
         var(ii,k)=(ttp(i5,j,k)-tm(i5,j))*sqrt(cos(phi))
1       continue
62      continue

CC*******Obtain the PCs********************

        open  (2, file='EOF',
     *     form='unformatted')

 	read(2)e 

	do 44 i=1,4
	do 44 n=1,nm
	c(i,n)=0.0
	do 45 m=1,ng
45	c(i,n)=c(i,n)+e(m,i)*var(m,n)
44	continue
	

	do k=1,nm
	y1(k)=c(1,k)
        end do

        call getstd(y1,nm,std)
        do k=1,nm
        y1(k)=y1(k)/std
        end do

CC Detrend
c       call detrend(y1,nm,a1,b1)
         do k=1,nm
c         y1(k)=y1(k)-a1*k-b1
          write(31,*)-y1(k)
         end do





      stop
      end



CC




#include "/zemo2/jiaxj/SUBROUTINES/getstd.f"
#include "/zemo2/jiaxj/SUBROUTINES/detrend.f"



