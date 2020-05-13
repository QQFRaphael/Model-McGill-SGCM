CC now changed to get 90 day average---

        parameter(nm=120,ns=nm,M=72,M1=M+1,N=37,Nrun=9)
        real x0(M1,N),x1(M1,N),xm(M1,N),x2(M1,N)


CC---

        OPEN(10,FILE='Z500_73.dat',  
     &form='UNFORMATTED',status='old')
CC----------------------

         read(10)x0



	do kk=1,5
        read(10)x1
	do i=1,73
	do j=1,37
 	  xm(i,j)=(x1(i,j)-x0(i,j))*100.
	end do
	end do

	  write(35) xm

	end do






      STOP
      END


