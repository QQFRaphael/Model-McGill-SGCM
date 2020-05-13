
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51*6)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)
      COMPLEX Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm),SPt(IGA,nm)


      OPEN(13,FILe='DFEOF3_OtoM.dat_crtd_ave',form='UNFORMATTED'
     & ,status='old')

      open(15,file='DFEOF3_OtoM.dat_crtd_reg',
     *    form='unformatted',status='old')

      open(16,file='ForceDHEOF3_Reg',
     *    form='unformatted')



         do k=1,nm

           read(15)ZA,DA,TA,SPA
	   read(13)Z0,D0,T0,SP0

          do I=1,IGB
           ZA(I)=ZA(I)+Z0(I)
           DA(I)=DA(I)+D0(I)
           TA(I)=TA(I)+T0(I)
          end do
          DO I=1,IGA
           SPA(I)=SPA(I)+SP0(I)
          END DO
	   write(16)ZA,DA,TA,SPA

         end do


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



