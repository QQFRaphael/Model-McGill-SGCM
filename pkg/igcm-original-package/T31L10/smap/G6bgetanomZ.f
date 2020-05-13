        parameter (nt=180,np=80)
        parameter (M=73, N=37,nday=180,L=15)
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
      Real y1(nday),y2(nday),y3(nday),y4(nday),y5(nday)
     &    ,y6(nday),y7(nday),y8(nday),y9(nday),y10(nday)
      Real fx1(nday),fx2(nday),fx3(nday),fx4(nday),fx5(nday)
     &    ,fx6(nday),fx7(nday),fx8(nday),fx9(nday),fx10(nday)
      Real th1(M,L,nday),th2(M,L,nday),th3(M,L,nday),th4(M,L,nday),
     &     th5(M,L,nday),th6(M,L,nday),th7(M,L,nday),th8(M,L,nday),
     &     th9(M,L,nday),th10(M,L,nday)
      Real ty1(M,L),ty2(M,L),ty3(M,L),ty4(M,L),
     &     ty5(M,L),ty6(M,L),ty7(M,L),ty8(M,L),
     &     ty9(M,L),ty10(M,L)


      open (32, file='anomZ.dat',form='unformatted',status='old')
      open (11, file='z50anomfilt.dat',form='unformatted')
      open (12, file='Z150anomfilt.dat',form='unformatted')
      open (13, file='Z250anomfilt.dat',form='unformatted')
      open (14, file='Z350anomfilt.dat',form='unformatted')
      open (15, file='Z450anomfilt.dat',form='unformatted')
      open (16, file='Z550anomfilt.dat',form='unformatted')
      open (17, file='Z650anomfilt.dat',form='unformatted')
      open (18, file='Z750anomfilt.dat',form='unformatted')
      open (19, file='Z850anomfilt.dat',form='unformatted')
      open (20, file='Z950anomfilt.dat',form='unformatted')

        do k1=1,np
	  print*,k1

        do 61 k2=1,nday

	 read(32)tl1
	 read(32)tl2
	 read(32)tl3
	 read(32)tl4
	 read(32)tl5
	 read(32)tl6
	 read(32)tl7
	 read(32)tl8
	 read(32)tl9
	 read(32)tl10
         do j=1,L
         do i=1,M
           tm1(i,j,k2)=tl1(i,j)
           tm2(i,j,k2)=tl2(i,j)
           tm3(i,j,k2)=tl3(i,j)
           tm4(i,j,k2)=tl4(i,j)
           tm5(i,j,k2)=tl5(i,j)
           tm6(i,j,k2)=tl6(i,j)
           tm7(i,j,k2)=tl7(i,j)
           tm8(i,j,k2)=tl8(i,j)
           tm9(i,j,k2)=tl9(i,j)
           tm10(i,j,k2)=tl10(i,j)
	 end do
	 end do


61      continue

	
         do 13 j=1,L
         do 13 i=1,M
	  do 62 k2=1,nday
	    y1(k2)=tm1(i,j,k2)
	    y2(k2)=tm2(i,j,k2)
	    y3(k2)=tm3(i,j,k2)
	    y4(k2)=tm4(i,j,k2)
	    y5(k2)=tm5(i,j,k2)
	    y6(k2)=tm6(i,j,k2)
	    y7(k2)=tm7(i,j,k2)
	    y8(k2)=tm8(i,j,k2)
	    y9(k2)=tm9(i,j,k2)
	    y10(k2)=tm10(i,j,k2)
62	  continue
	 call lfilter(y1,fx1,nday)
	 call lfilter(y2,fx2,nday)
	 call lfilter(y3,fx3,nday)
	 call lfilter(y4,fx4,nday)
	 call lfilter(y5,fx5,nday)
	 call lfilter(y6,fx6,nday)
	 call lfilter(y7,fx7,nday)
	 call lfilter(y8,fx8,nday)
	 call lfilter(y9,fx9,nday)
	 call lfilter(y10,fx10,nday)

	  do k=1,nday  	
	    th1(i,j,k)=fx1(k)	    
	    th2(i,j,k)=fx2(k)	    
	    th3(i,j,k)=fx3(k)	    
	    th4(i,j,k)=fx4(k)	    
	    th5(i,j,k)=fx5(k)	    
	    th6(i,j,k)=fx6(k)	    
	    th7(i,j,k)=fx7(k)	    
	    th8(i,j,k)=fx8(k)	    
	    th9(i,j,k)=fx9(k)	    
	    th10(i,j,k)=fx10(k)	    
	  end do  

13	  continue	


	 do k=1,nday
         do j=1,L
         do i=1,M
          ty1(i,j)=th1(i,j,k)
          ty2(i,j)=th2(i,j,k)
          ty3(i,j)=th3(i,j,k)
          ty4(i,j)=th4(i,j,k)
          ty5(i,j)=th5(i,j,k)
          ty6(i,j)=th6(i,j,k)
          ty7(i,j)=th7(i,j,k)
          ty8(i,j)=th8(i,j,k)
          ty9(i,j)=th9(i,j,k)
          ty10(i,j)=th10(i,j,k)
	 end do
	 end do
	  write(11) ty1
	  write(12) ty2
	  write(13) ty3
	  write(14) ty4
	  write(15) ty5
	  write(16) ty6
	  write(17) ty7
	  write(18) ty8
	  write(19) ty9
	  write(20) ty10
	 end do



	 end do












	  stop
	  end



#include "filter-jxj.h"
