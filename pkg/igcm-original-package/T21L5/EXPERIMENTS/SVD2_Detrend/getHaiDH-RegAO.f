	PARAMETER ( mg=64,jg2=32,nl1=7,nl2=5,nm=51)
	REAL UDP(mg,jg2,nl2),VDP(mg,jg2,nl2),TDP(mg,jg2,nl2),
     &SPDP(mg,jg2)

	real pot(mg,jg2),x(73,37),fll(73,37),pot1(mg,jg2)
	real tf(mg,jg2,nm),y1(nm),y2(nm),c(nm)

        open(12,file='/zemo2/jiaxj/TroSST2AO/SVD/SVD2SSTindex.dat')
        do k=1,nm
         read(12,*) y1(k)
        end do

cc detrend
        call detrend(y1,nm,aa1,b1)
        do k=1,nm
          y1(k)=y1(k)-aa1*k-b1
        end do

        call getstd(y1,nm,std)
        print*,std

CCCCCCCCCCCCCC




	OPEN(UNIT=25,FILE='/diska/hlin/igcm/paper10/data/DIAB_51DJF.dat',
     &  STATUS='OLD',FORM='UNFORMATTED')

	do iy=1,nm
	do k=1,10
         read(25)pot
        end do

	do i=1,mg
        do j=1,jg2
          pot1(i,j)=0.
        end do
        end do

	do k=1,5
	 read(25)pot
	do i=1,mg
	do j=1,jg2
 	  pot1(i,j)=pot1(i,j)+pot(i,j)/5.
	end do
	end do
	end do

	do i=1,mg
        do j=1,jg2
          tf(i,j,iy)=pot1(i,j)
        end do
        end do

CCskip records
        do kk=1,6
         read(25)
        end do

	end do

CC
	do i=1,mg
	do j=1,jg2
	 do k=1,nm
	 y2(k)=tf(i,j,k)
	 end do
	 call regress(y1,y2,nm,a,b)
         pot(i,j)=a*std
	end do
	end do
CC
cc Convert to lat-lon grid
         call gautogrid(pot,fll)
	 do i=1,73
	 do j=1,37
	 x(i,j)=fll(i,j)*12.
	 end do
	 end do


	write(16)x
	
        stop
        end


#include "/diska/hlin/qgmodel/plot/gausstogrid.f"

        subroutine regress(x,y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.

        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end
CC
        subroutine getstd(y,nm,r)
        real y(nm)
        s=0.
        do k=1,nm
        s=s+y(k)/float(nm)
        end do

        r=0.
        do k=1,nm
        r=r+(y(k)-s)**2
        end do

        r=sqrt(r/float(nm-1))

        return
        end


CCCCCCCCCC
        subroutine detrend(y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
        do i=1,nm
        x(i)=i
        end do
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.
        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end



CCCCCCCCCCCCCC

