      PROGRAM GETFORCE
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX az(IGB),bz(IGB),ad(IGB),bd(IGB),at(IGB),
     *      bt(IGB),ap(IGA),bp(IGA)


	real y1(nm)
        open(12,file='/diskc/jiaxj/ncarplot/SVD/S2t_detrend.dat')
        do k=1,nm
         read(12,*)y1(k)
        end do
CC 
	OPEN(16,file='regCoeff-SVD1.dat',form='UNFORMATTED')
	read(16)az,bz,ad,bd,at,bt,ap,bp

	do k=1,nm
	 DO I=1,IGB
	 ZA(I)=az(I)*y1(k)+bz(I)
	 DA(I)=ad(I)*y1(k)+bd(I)
	 TA(I)=at(I)*y1(k)+bt(I)
	 END DO
	 DO I=1,IGA
	 SPA(I)=ap(I)*y1(k)+bp(I)
	 END DO

        OPEN(15,FILE='Force_SVD2_51DJF',form='UNFORMATTED')

	write(15)ZA,DA,TA,SPA

	end do

      STOP
      END               
