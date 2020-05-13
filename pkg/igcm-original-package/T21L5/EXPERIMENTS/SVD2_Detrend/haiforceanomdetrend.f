	PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51,ne=15)

      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)


	COMPLEX a1,b1,a2,a3,b2,b3
	COMPLEX Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm)
        COMPLEX SPt(IGA,nm), z1(nm),z2(nm),z3(nm)
	COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)
	real c(4,nm),y1(nm)

	open(12,file='sstPacDJFeffic.dat',
     *     form='unformatted')
        read(12)c
	do k=1,nm
	 y1(k)=c(2,k)
	end do
cdetrend
         call detrend(y1,nm,a1,b1)
         do k=1,nm
          y1(k)=y1(k)-a1*k-b1
         end do


	call getstd(y1,nm,std)

	OPEN(14,FILE='/diska/hlin/igcm/force/FORCE_51DJF.dat_crtd'
     &       ,form='UNFORMATTED',
     *  status='old')
 	OPEN(15,FILE='FORCE_anomRegE2',form='UNFORMATTED')

 	OPEN(16,file='regCoeff-E2-detrend.dat',form='UNFORMATTED')

	do iy=1,nm
	 print*,iy
	READ(14)Z,D,T,SP
	
	do i=1,IGB
	Zt(I,iy)=Z(I)
        Dt(I,iy)=D(I)
        Tt(I,iy)=T(I)
        ENDDO
        DO I=1,IGA
        SPt(I,iy)=SP(I)
        ENDDO
	end do

CC
	do i=1,IGB
	 do k=1,nm
	  z1(k)=Zt(I,k)
          z2(k)=Dt(I,k)
          z3(k)=Tt(I,k)
         end do
	 call regress(y1,z1,nm,a1,b1)
	 call regress(y1,z2,nm,a2,b2)
	 call regress(y1,z3,nm,a3,b3)

CC without b, it's anomaly, b is z average
         ZA(I)=a1*std
	 DA(I)=a2*std
	 TA(I)=a3*std
	 az(I)=a1
	 ad(I)=a2
	 at(I)=a3
	 bz(I)=b1
	 bd(I)=b2
	 bt(I)=b3
	end do

	DO I=1,IGA
         do k=1,nm
          z1(k)=SPt(I,k)
         end do
         call regress(y1,z1,nm,a1,b1)
	 SPA(I)=a1*std
	 ap(I)=a1
	 bp(I)=b1
        END DO
CC
        write(15)ZA,DA,TA,SPA
 	write(16)az,bz,ad,bd,at,bt,ap,bp
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

