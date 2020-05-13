
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51*6)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX ZAA(IGB,nm),DAA(IGB,nm),TAA(IGB,nm),SPAA(IGA,nm)
      COMPLEX ZAB(IGB),DAB(IGB),TAB(IGB),SPAB(IGA)
      COMPLEX Z0(IGB,6),D0(IGB,6),T0(IGB,6),SP0(IGA,6)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)
      COMPLEX Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm),SPt(IGA,nm)

      REAL c1(nm),c2(nm),c3(nm)
      real y1(nm)
      COMPLEX a1,b1,a2,a3,b2,b3,a4,b4
      COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)
     *      ,z3(nm),z1(nm),z2(nm),z4(nm)

        print*,'Remember to use -r8'

      open(14,file='/zemo2/jiaxj/EOF/cindex',
     *    form='unformatted',status='old')

      OPEN(13,FILe='DFEOF3_OtoM.dat_crtd',form='UNFORMATTED'
     & ,status='old')

      open(15,file='DFEOF3_OtoM.dat_crtd_reg',
     *    form='unformatted')

        read(14)c1
        read(14)c2
        read(14)c3
        do k=1,nm
         y1(k)=c3(k)
        end do

cc detrend
        call detrend(y1,nm,a1,b1)

        do k=1,nm
          y1(k)=y1(k)-a1*k-b1
        end do

        call getstd(y1,nm,std)
        print*,std
	

        do k=1,nm
          y1(k)=y1(k)/std
        end do

        do iy=1,nm
          READ(13)Z,D,T,SP
        do I=1,IGB
          Zt(I,iy)=Z(I)
          Dt(I,iy)=D(I)
          Tt(I,iy)=T(I)
        ENDDO
        DO I=1,IGA
          SPt(I,iy)=SP(I)
        ENDDO
        end do



        do I=1,IGB
         do k=1,nm
          z1(k)=Zt(I,k)
          z2(k)=Dt(I,k)
          z3(k)=Tt(I,k)
         end do

         call regress(y1,z1,nm,a1,b1)
         call regress(y1,z2,nm,a2,b2)
         call regress(y1,z3,nm,a3,b3)

C without b, it's anomaly, b is z average
         ZA(I)=a1
         DA(I)=a2
         TA(I)=a3
        end do


        DO I=1,IGA
         do k=1,nm
          z4(k)=SPt(I,k)
         end do
         call regress(y1,z4,nm,a4,b4)
         SPA(I)=a4
        END DO

c       OPEN(35,FILe='RegField',form='UNFORMATTED')
c       write(35)ZA,DA,TA,SPA

         do k=1,nm
         do I=1,IGB
           ZAA(I,k)=ZA(I)*y1(k)
           DAA(I,k)=DA(I)*y1(k)
           TAA(I,k)=TA(I)*y1(k)
         end do
         DO I=1,IGA
           SPAA(I,k)=SPA(I)*y1(k)
         END DO
	 end do

         do k=1,nm
         do I=1,IGB
           ZAB(I)=ZAA(I,k)
           DAB(I)=DAA(I,k)
           TAB(I)=TAA(I,k)
         end do
         DO I=1,IGA
           SPAB(I)=SPAA(I,k)
         END DO

	  write(15)ZAB,DAB,TAB,SPAB

         end do
	
		 
 





	print*,' Good Job!'



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



