        parameter ( nm=180*45, ng=36*14,nt=180,np=45)
        parameter ( M=73, N=37,nday=180,L=15)
        Real t(M,N),tm(M,L,nday),e(ng,4),c(4,nm),yy1(nm)
	real ttp(M,L,nm),t1(M,N),t2(M,L,nm)
	real y1(nm),dex(np,nt),dexx(np,nt)
	real th(M,L,nday),ty(M,L),tl(M,L)
	real fx(nday)


      open (12, file='anom.dat',form='unformatted',status='old')
      open (15, file='SLPanomfilt.dat',form='unformatted')

        do k1=1,np
	  print*,k1

        do 61 k2=1,nday
	 read(12)tl
         do 12 j=1,L
         do 12 i=1,M
12       tm(i,j,k2)=tl(i,j)
61      continue

	
         do 13 j=1,L
         do 13 i=1,M
	  do 62 k2=1,nday
	    y1(k2)=tm(i,j,k2)
62	  continue
	 call lfilter(y1,fx,nday)

	  do k=1,nday  	
	    th(i,j,k)=fx(k)	    
	  end do  

13	  continue	


	 do k=1,nday
         do 14 j=1,L
         do 14 i=1,M
14        ty(i,j)=th(i,j,k)
	  write(15) ty
	 end do




	 end do












	  stop
	  end



#include "filter-jxj.h"
