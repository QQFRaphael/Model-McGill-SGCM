
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX Z00(IGB),D00(IGB),T00(IGB),SP00(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)

      REAL*8 c(nm),y1(nm),z3(nm),z1(nm),z2(nm),z4(nm)
      COMPLEX a1,b1,a2,a3,b2,b3,a4,b4
      COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)



      OPEN(13,FILe='DFEOF3_OtoM.dat_crtd',form='UNFORMATTED'
     & ,status='old')

      OPEN(17,FILe='DFEOF3_OtoM.dat_crtd_clim',form='UNFORMATTED')

	print*,'Remember to use -r8.....'


CC  climate for each month

	
        do k=1,6
        do i=1,IGB
         Z0(i)=0.
         D0(i)=0.
         T0(i)=0.
        end do
        do i=1,IGA
         SP0(i)=0.
        end do
        end do

	do kk=1,nm

           read(13)Z,D,T,SP
         do i=1,IGB
           Z0(i)=Z0(i)+Z(i)/real(nm)
           D0(i)=D0(i)+D(i)/real(nm)
           T0(i)=T0(i)+T(i)/real(nm)
         end do
         do i=1,IGA
           SP0(i)=SP0(i)+SP(i)/real(nm)
         end do

	end do

	 write(17)Z0,D0,T0,SP0

	
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



