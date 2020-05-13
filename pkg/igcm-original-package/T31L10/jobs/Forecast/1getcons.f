C
CC get initial confdition
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 ZI(IGB),DI(IGB),TI(IGB),SPI(IGA)
      COMPLEX*16 ZM(IGB),DM(IGB),TM(IGB),SPM(IGA)
      COMPLEX*16 aa,bb,cc,dd
	
	
	REAL*8 RKOUNT,RMTAPE,DAY

CCC 13 is the original initial condition, 11 is the climatology.
        OPEN(11,
     & FILE='OUTpred_con.dat',
     &form='UNFORMATTED')

         DO I=1,IGB
          ZA(I)=0.0005*(-1)**(I)
          DA(I)=0.0005*(-1)**(I)
          TA(I)=0.0005*(-1)**(I)
         ENDDO
         DO I=1,IGA
          SPA(I)=0.0005*(-1)**(I)
         ENDDO

      RKOUNT=0.
      DAY=0.
        WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE


      STOP
      END
