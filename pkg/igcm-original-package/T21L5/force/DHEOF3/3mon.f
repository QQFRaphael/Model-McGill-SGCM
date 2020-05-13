
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51*6)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX Z1(IGB),D1(IGB),T1(IGB),SP1(IGA)



      OPEN(17,FILe='DFEOF3_OtoM.dat_crtd_ave',form='UNFORMATTED')
      OPEN(13,FILe='DFEOF3_OtoM.dat_crtd_3mon',form='UNFORMATTED')

	print*,'Remember to use -r8.....'



	 
           read(17)Z1,D1,T1,SP1
           read(17)Z1,D1,T1,SP1

	  do ii=1,3
           read(17)Z,D,T,SP
           write(13)Z,D,T,SP
	  end do

           read(17)Z1,D1,T1,SP1


	
	print*,'Good Job!'

	stop
	end



        subroutine regress(x,y,nm,a,b)
        real x(nm)
        COMPLEX y(nm),a,b,y1,xy
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


C------------------------------
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



