C
CC get initial confdition
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 ZI(IGB),DI(IGB),TI(IGB),SPI(IGA)
      COMPLEX*16 ZM(IGB),DM(IGB),TM(IGB),SPM(IGA)
	REAL*8 RKOUNT,RMTAPE,DAY


CCC 13 is the original initial condition, 11 is the climatology.

        OPEN(12,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Test/OUTpred_total.dat',
     &form='UNFORMATTED')
        OPEN(11,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Test/OUTpred_clim.dat',
     &form='UNFORMATTED')
        OPEN(13,
     & FILE='OUTpred_pert5.dat',form='UNFORMATTED')

         READ(11)RKOUNT,RMTAPE,DAY,ZM,DM,TM,SPM,RMTAPE
	 do k=1,30
 	 READ(12)
	 end do
	 do k=1,2000
 	 READ(12)RKOUNT,RMTAPE,DAY,ZI,DI,TI,SPI,RMTAPE

         DO I=1,IGB
       ZA(I)=(ZI(I)-ZM(I))/5.
       DA(I)=(DI(I)-DM(I))/5.
       TA(I)=(TI(I)-TM(I))/5.
       ENDDO
       DO I=1,IGA
       SPA(I)=(SPI(I)-SPM(I))/5.
       ENDDO

      RKOUNT=0.
      DAY=0.
        WRITE(13)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE

	end do

      STOP
      END
