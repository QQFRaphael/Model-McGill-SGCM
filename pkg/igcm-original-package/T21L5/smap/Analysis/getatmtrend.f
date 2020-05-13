        parameter (M=73, N=37,nm=51)

        real aa(M,N),bb(M,N),aa1(M,N),aa2(M,N),cc(M,N)
        real tmp(M,N,nm),var(36,14)
	real z1(nm),a1,b1

 	OPEN(14,FILE='SLP.dat'
     & ,form='UNFORMATTED',status='old')

 	OPEN(20,file='atmtrend.dat',form='UNFORMATTED')



	do k=1,nm
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
         cc(i,j)=a1*51
	 end do
	 end do


CC 80N-20N
        do 1 j=2,15
        do 1 i=1,36
          i5=2*(i-1)+1
          var(i,j)=cc(i5,j)
1       continue



        write(20)var



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

