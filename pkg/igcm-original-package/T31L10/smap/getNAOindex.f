        PARAMETER (mg=73,jg=15,nm=180*80)

        real y1(nm), y2(nm),yy(nm),y3(nm),y4(nm),y5(nm)
        real sp(mg,jg),xx(51),fx(180)

         open(12, 
     & file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/SLPanomfilt.dat',
     &		form='unformatted')
	 open(34, 
     &  file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/NAOdailyindex')

         ss1=0.
         ss2=0.

        do k=1,nm
         read(12)sp
         ss1=ss1+sp(69,6)/real(nm)
         ss2=ss2+sp(71,11)/real(nm)
        end do
	rewind(12)

         do k=1,nm
         read(12)sp
	 y1(k)=sp(69,6)-ss1
	 y2(k)=sp(71,11)-ss2
        end do

	call getstd(y1,nm,std1)
	call getstd(y2,nm,std2)
 
	do k=1,nm
         y1(k)=y1(k)/std1
         y2(k)=y2(k)/std2
	end do 

	do k=1,nm
	yy(k)=y2(k)-y1(k)
	end do

	do k2=1,51
	do k1=1,180
	 y4(k1)=yy(k1+180*(k2-1))
	end do
	 call lfilter(y4,fx,180)
	do k1=1,180
	 y5(k1+180*(k2-1))=fx(k1)
	end do	
	end do

        call getstd(y5,nm,std3)

	do k=1,nm
	write(34,*) y5(k)/real(std3)
	end do



CC Do seasonal ave for the index
	do k2=1,51
	ss=0.
	do k1=1,180
	ss=ss+yy(k1+180*(k2-1))/real(180)
	end do
	xx(k2)=ss
	end do
	
	do k=1,51
c	write(33,*) xx(k)
	end do

c      call getstd(xx,51,std1)


	stop
        end


# include "/zemo2/jiaxj/SUBROUTINES/getstd.f"
#include "/zemo2/jiaxj/SUBROUTINES/filter-jxj.h"

