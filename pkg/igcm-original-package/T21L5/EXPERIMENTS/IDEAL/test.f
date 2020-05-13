	PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)

      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 ZC(IGB),DC(IGB),TC(IGB),SPC(IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE


 	OPEN(16,file='force_clim.dat',form='UNFORMATTED')
        OPEN(15,file='force_ideal.dat',form='UNFORMATTED')

	READ(15)Z,D,T,SP
	READ(16)ZA,DA,TA,SPA
	
	do i=1,IGB
	ZC(I)=Z(I)-ZA(I)
        DC(I)=D(I)-DA(I)
        TC(I)=T(I)-TA(I)
        ENDDO
        DO I=1,IGA
        SPC(I)=SP(I)-SPA(I)
        ENDDO

CC
CC

        write(30)ZC,DC,TC,SPC
        stop
        end



