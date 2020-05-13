        parameter ( nm=180*80, ng=36*14,nt=180,ns=180,np=80)
        parameter ( M=73, N=37,L=15,nday=180)
        real ttp(M,L,nm),t1(M,N),t2(M,L,nm)
        real ylf(nm),ts(M,L,nm),fx(nm)
        real y3(500),y2(500),aa1(500),bb1(500)
	real y1(500),y4(500),aa2(500),bb2(500)


      open (12, 
     &file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/AOdailyindex')


	do kk=1,nm
	 read(12,*) fx(kk)
	end do

        call selpos(fx,nday,np,itp,aa1,bb1)
	call exam(itp,aa1,bb1,y1,y2,nn)
	
	   do kk=1,nn
           write(35,*) y1(kk),y2(kk)
	   end do 



 	call selneg(fx,nday,np,itn,aa2,bb2)
	call exam(itn,aa2,bb2,y3,y4,mm)
	   do kk=1,mm
           write(36,*) y3(kk),y4(kk)
	   end do






	   stop
	   end


#include "selneg.h"
#include "selpos.h"
#include "exam.h"



