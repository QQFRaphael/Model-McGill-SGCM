      PROGRAM GETFORCE
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 Zc(IGB),Dc(IGB),Tc(IGB),SPc(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm),SPt(IGA,nm)
      COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)



	OPEN(15,file='haiclimate',form='UNFORMATTED',status='old')
	read(15)Zc,Dc,Tc,SPc
	OPEN(16,file='Trend.dat',form='UNFORMATTED',status='old')
	read(16)Z,D,T,SP

	 DO I=1,IGB
	 Z(I)=Z(I)+Zc(I)
	 D(I)=D(I)+Dc(I)
	 T(I)=T(I)+Tc(I)
	 END DO
	 DO I=1,IGA
	 SP(I)=SP(I)+SPc(I)
	 END DO

         write(18)Z,D,T,SP


      STOP
      END               



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


