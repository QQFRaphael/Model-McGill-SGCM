	parameter(nm=36*14)
	real y1(nm),y2(nm),c(nm),c1(nm)
C
        open(12,file='fort.30')
 	open(13,file='fort.31')

	do k=1,nm
	read(12,*) y1(k)
 	read(13,*) y2(k)
	end do


	call pearsn(y1,y2,51,c0,prob,z)
	
 	print*,c0,prob
c	write(*,'(i20,f10.2,f10.5)')m,c0,prob

	s1=0.
	s2=0.
	r1=0.
	r2=0.
	do k=1,nm
	s1=s1+y1(k)/float(nm)
	s2=s2+y2(k)/float(nm)
	end do

	do k=1,nm
	r1=r1+(y1(k)-s1)**2/float(nm-1)
	r2=r2+(y2(k)-s2)**2/float(nm-1)
	end do
	r1=sqrt(r1)
	r2=sqrt(r2)
c	print*, r1, r2, r2/r1 
	do k=1,51
 	write(88,*) 1948+k, (y1(k)-s1)/r1,(y2(k)-s2)/r2
c	write(88,*)(y1(k)-s1)/r1,(y2(k)-s2)/r2
	end do

	end

#include "/diskc/hlin/recipes/pearsn.for"
#include "/diskc/hlin/recipes/betai.for"
#include "/diskc/hlin/recipes/betacf.for"
#include "/diskc/hlin/recipes/gammln.for"
