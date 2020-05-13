        parameter ( nm=180*80, ng=36*14,nt=180,ns=180,np=80)
        parameter ( M=73, N=37,L=15,nday=180)
      Real t(M,N),tm(M,L),e(ng,4),c(4,nm),
     &      var(ng,nm), cct(ng,ng),fllp(M,N)
	real ttp(M,L,nm),t1(M,N),t2(M,L,nm)
	real y1(nm),dex(np,nt),dexx(np,nt)
	real ylf(nm),tt(nt),ts(M,L,nm),fx(nm)
	real y3(nm),y2(nm)
	real tl(M,L)


      open (11, 
     *file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/SLPanomfilt.dat'
     *,form='unformatted', status='old')
      open (12, 
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/AOdailyindex')

	rewind(11)

	do 62 k=1,nm
          ii=0
CC 20N-85N
	 read(11) tl


        do 1 j=2,L
         phi=90.-(j-1)*5.
         phi=phi*3.1415926/180.
        do 1 i=1,36
          ii=ii+1
          i5=2*(i-1)+1
          var(ii,k)=(tl(i5,j))*sqrt(cos(phi))
1       continue
62      continue

CC*******Obtain the PCs********************

        open  (2, file='/zemo2/jiaxj/data/Monthlymean/PRES.OtoM.EOF',
c       open  (2, file='EOF',
     *     form='unformatted')

 	read(2)e 

	do 44 i=1,4
	do 44 nn=1,nm
	c(i,nn)=0.0
	do 45 mm=1,ng
45	c(i,nn)=c(i,nn)-e(mm,i)*var(mm,nn)
44	continue
CC Since the AO pattern is negative phase

	do k=1,nm
	y1(k)=c(1,k)
        end do



cc detrend
         call detrend(y1,nm,a1,b1)
         do k=1,nm
          y1(k)=y1(k)-a1*k-b1
         end do

	call getstd(y1,nm,std)
CCCCCCCC

         do k=1,nm
          y1(k)=y1(k)/real(std)
	  write(12,*) y1(k)
        end do


        call getstd(y1,nm,std3)
        print*, std3






	 stop 
	 end 




#include "/zemo2/jiaxj/SUBROUTINES/filter-jxj.h"
#include "/zemo2/jiaxj/SUBROUTINES/detrend.f"
#include "/zemo2/jiaxj/SUBROUTINES/getstd.f"

