        parameter (M=72, N=37,nm=51,M1=73)

        real aa(M,N),bb(M,N),aa1(M,N),aa2(M,N),cc(M1,N)
        real tmp(M,N,nm)
	real z1(nm),a1,b1

c	OPEN(14,FILE='/diskc/jiaxj/force/HaiForce/Pmsl.dat'
 	OPEN(14,FILE='/diskc/jiaxj/force/HaiForce/Z500.dat'
     & ,form='UNFORMATTED',status='old')

c	OPEN(16,file='SLPtrend.dat',form='UNFORMATTED')
 	OPEN(16,file='Z500trend.dat',form='UNFORMATTED')

	do k=1,nm
	 print*,k
         READ(14)aa
         do i=1,M
         do j=1,N
	   tmp(i,j,k)=aa(i,j)
	 end do
	 end do
	end do


CC
         do i=1,M
         do j=1,N
	 do k=1,nm
	  z1(k)=tmp(i,j,k)
         end do

	 call detrend(z1,nm,a1,b1)
CC a is the trend part.
         cc(i,j)=a1
	 end do
	 end do

	do 40 j=1,37
	  cc(73,j)=cc(1,j)
40	continue

        write(16)cc
        stop
        end




CCCCCCCCCCC

        subroutine detrend(y,nm,a,b)
        real y(nm),a,b
	real x(nm)
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

